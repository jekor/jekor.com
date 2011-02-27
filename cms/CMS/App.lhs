\section{The App Monad}
\label{App}

> module CMS.App (             App, AppEnv(..), AppT, runApp, getOption,
>                              output404, reversibleRedirect,
>                              queryTuple', queryTuples', execute',
>  {- Control.Monad.Reader -}  asks) where

> import CMS.CGI
> import CMS.Utils

> import Control.Applicative (Applicative)
> import Control.Monad (ap)
> import Control.Monad.Error (runErrorT)
> import Control.Monad.Reader (ReaderT(..), MonadReader, asks)
> import Control.Monad.Trans (lift)

> import Data.ConfigFile (ConfigParser, get)
> import Data.List (intercalate)
> import Language.Haskell.TH (Q, Exp)
> import Network.CGI.Monad (MonadCGI(..))
> import Network.CGI (CGI, CGIT, outputNotFound)
> import Network.URI (escapeURIString, isUnescapedInURI)

> data AppEnv = AppEnv {  appDB          :: Handle,
>                         appCP          :: ConfigParser }

The App monad is a combination of the CGI and Reader monads.

> newtype AppT m a = AppT (ReaderT AppEnv (CGIT m) a)
>   deriving (Monad, MonadIO, MonadReader AppEnv)

...whose CGI monad uses the IO monad.

> type App = AppT IO

We need to make the App monad an Applicative Functor so that it will work with
formlets.

> instance Applicative App where
>   pure = return
>   (<*>) = ap

> instance Functor App where
>   fmap = liftM

To make the App monad an instance of MonadCGI, we need to define basic CGI
functions. CGI is relatively simple and its functionality can be defined on top
of just an environment getter and a function for adding headers. We reuse the
existing methods.

> instance MonadCGI App where
>   cgiAddHeader n = AppT . lift . cgiAddHeader n
>   cgiGet = AppT . lift . cgiGet

|runApp| does the job of creating the Reader environment and returning the
CGIResult from within the App monad to the CGI monad.

> runApp :: Handle -> ConfigParser -> App CGIResult -> CGI CGIResult
> runApp h cp (AppT a) = do
>   res <- runReaderT a $ AppEnv {  appDB          = h,
>                                   appCP          = cp }
>   return res

\subsection{Convenience Functions}

Here are some functions that abstract away even having to ask for the
environment in the App monad.

When we direct a user to the some page, we might want to make sure that they
can find their way back to where they were. To do so, we get the current URI
and append it to the target page in the query string. The receiving page might
know what to do with it.

> reversibleRedirect :: String -> App String
> reversibleRedirect path = do
>   request <- fromMaybe "/" `liftM` getVar "REQUEST_URI"
>   return $ path ++ "?redirect=" ++ escapeURIString isUnescapedInURI request

We want to log 404 errors in the database, as they may indicate a problem or
opportunity with the site. This takes a list of Strings that are stored in the
log. It outputs to the user the request URI.

> output404 :: [String] -> App CGIResult
> output404 s = outputNotFound $ intercalate "/" s

\subsubsection{Database}

When we're dealing with the database, there's always a chance we're going to
have some sort of error (there's a seemingly infinite number of possible
sources). We don't want the entire page to blow up if there are. Also, we don't
really care what the cause of the error is at the time of execution. SQL errors
are not something you can generally recover from. We just need to log the
error, return some sort of error indicator to the calling function (in this
case, Nothing), and get on with it.

In many cases, the calling function will still need to do data validation
anyway (make sure that a list of the expected size is returned, etc), so the
extra Maybe wrapper shouldn't be much extra trouble. In fact, in some cases
it's much easier than manually wrapping the query with |catchSql|.

-- Here are some convenience functions for working with the database in the App
-- monad.

> withConnection :: (Handle -> IO a) -> App a
> withConnection action = do h <- asks appDB
>                            liftIO $ action h

> queryTuple' :: String -> Q Exp
> queryTuple' sql = [| withConnection $(queryTuple sql) |]

> queryTuples' :: String -> Q Exp
> queryTuples' sql = [| withConnection $(queryTuples sql) |]

> execute' :: String -> Q Exp
> execute' sql = [| withConnection $(execute sql) |]

\subsubsection{Exceptions}

|tryApp| is like |tryCGI|. It allows us to catch exceptions within the App
monad. To do so, we unwrap the Reader monad and use TryCGI (which unwraps
another Reader and Writer).

> tryApp :: App a -> App (Either SomeException a)
> tryApp (AppT c) = AppT (ReaderT (tryCGI' . runReaderT c))

\subsubsection{Configuration}

Return a configuration option or log an error.

This always pulls from the DEFAULT section. It also only supports strings.

> getOption :: String -> App (Maybe String)
> getOption option = do
>   cp <- asks appCP
>   opt <- runErrorT $ get cp "DEFAULT" option
>   case opt of
>     Left _   -> return Nothing
>     Right o  -> return $ Just o
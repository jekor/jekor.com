\documentclass[oneside,draft]{article}
%include polycode.fmt
\usepackage[T1]{fontenc}
\usepackage{ucs}
\usepackage[utf8]{inputenc}
\usepackage{hyperref}
\usepackage[pdftex]{graphicx}
\usepackage[x11names, rgb]{xcolor}
\usepackage{tikz}
\usetikzlibrary{decorations,arrows,shapes}
\usepackage[margin=1.4in]{geometry}

\hypersetup{colorlinks=true}

\title{CMS}
\author{Chris Forno (jekor)}
\date{April 21, 2009}

\begin{document}
\maketitle

\subsection{Architecture}

Requests arrive via a webserver. They are passed to the cms.fcgi process (this
program) on TCP port 10033.

Upon receiving a request (connection), we immediately fork a new thread. In
this thread, we establish a connection to a PostgreSQL server (for each
request). We then examine the thread for an authentication cookie. If it exists
and is valid, we assume that the request is from an authenticated member. We
pack both the database handle and the authenticated member information into our
``App'' monad (\autoref{App}).

> module Main where

\section{Our Modules}

These are the Vocabulink modules. They are grouped primarily based on division
of labor. The exception is the App module. The App module defines the App monad
and must make use of both database and CGI functions. In order to limit
cyclical dependencies (which can be a pain with the GHC compiler), it's broken
out into a separate module.

> import CMS.App

Each of these modules will be described in its own section.

> import CMS.Article
> import CMS.DB
> import CMS.CGI
> import CMS.Html hiding (method, options)
> import CMS.Utils

\section{Other Modules}

CMS makes use of a half dozen or so Haskell libraries. Even though we
don't use them all in this module, I'll describe them here so that they'll be
more familiar as they're introduced (and so that you can jump directly to the
section you're interested in after this introduction).

\begin{description}

\item[Codec.Binary.UTF8.String] CMS would be pretty useless without
being able to handle the writing systems of other languages. We only make use
of 2 functions provided by this library: |encodeString| and |decodeString|.
|decodeString| takes a UTF-8 string---either from the webserver or from the
database---and converts it into a Unicode string that can be used by Haskell
natively. We use |encodeString| to go in the other direction. Whenever we write
out a string to the database, the webserver, or a log file; it needs to be
encoded to UTF-8. This is something that the type system does not (yet) handle
for us, so we need to be careful to correctly encode and decode strings.

\item[Data.ConfigFile] We need to have some parameters configurable at runtime.
This allows us to do things differently in test and production environments. It
also allows us to publish the source to the program without exposing sensitive
information.

\item[Network.URI] Various parts of the code may need to construct or
deconstruct URLs. Using this library should be safer than using various
string-mangling techniques throughout the code.

\item[Text.ParserCombinators.Parsec] We need to parse text quite a bit. The
dispatcher, the member authentication routines, and the article publishing
system all make use of Parsec; and probably more will in the future.

\end{description}

There are a few more, but they are only used by a single CMS
module\footnote{The CMS module may re-export some functions provided by the
module, but the other CMS modules should be able to remain ignorant of that.}.

> import Control.Concurrent (forkIO)
> import Control.Monad (join)
> import Control.Monad.Error (runErrorT)
> import Data.ConfigFile (readfile, emptyCP, ConfigParser, CPError, options)
> import Data.List (find, intercalate, intersect)
> import Data.List.Split (splitOn)
> import Network.FastCGI (runFastCGIConcurrent')
> import Network.URI (URI(..), unEscapeString)

\section{Entry and Dispatch}

When the program starts, it immediately begin listening for connections.
|runFastCGIConcurrent'| spawns up to 10 threads. |handleErrors'| and |runApp|
will be explained later. The basically catch unhandled database errors and pack
information into the App monad.

Before forking, we read a configuration file. We pass this to runApp so that
all threads have access to global configuration information.

The first thing we do after forking is establish a database connection. The
database connection might be used immediately in order to log errors. It'll
eventually be passed to the App monad where it'll be packed into a reader
environment.

> main :: IO ()
> main = do  cp' <- getConfig
>            case cp' of
>              Left e    -> print e
>              Right cp  -> runFastCGIConcurrent' forkIO 2048 (do
>                c <- liftIO connect
>                handleErrors' c (runApp c cp handleRequest))

The |configFile| is the one bit of configuration that's the same in all
environments.

> configFile :: String
> configFile = "/etc/cms.conf"

These config vars are required for CMS to do anything useful. We check
for them at load time and they can be safely read later with |forceEither $
get|.

> requiredConfigVars :: [String]
> requiredConfigVars = ["dbpass", "articledir"]

This retrieves the config file and makes sure that it contains all of the
required configuration parameters. This is so that we find out about errors
when starting the program rather than in individual threads later.

> getConfig :: IO (Either CPError ConfigParser)
> getConfig = runErrorT $ do
>   cp <- join $ liftIO $ readfile emptyCP configFile
>   opts <- options cp "DEFAULT"
>   if intersect requiredConfigVars opts == requiredConfigVars
>      then return cp
>      else error "Missing configuration options."

|handleRequest| ``digests'' the requested URI before passing it to the
 dispatcher. It also sets the response header. If we ever serve up non-HTML
 content, the header will need to be set at a lower level.

> handleRequest :: App CGIResult
> handleRequest = do
>   uri     <- requestURI
>   method  <- requestMethod
>   let path = pathList uri
>   dispatch' method path

We extract the path part of the URI, ``unescape it'' (convert % codes back to
characters), decode it (convert \mbox{UTF-8} characters to Unicode Chars), and finally
parse it into directory and filename components.

\begin{quote}@/some/directory/and/a/filename@\end{quote}

becomes

\begin{quote}|["some","directory","and","a","filename"]|\end{quote}

Note that the parser does not have to deal with query strings or fragments
because |uriPath| has already stripped them.

The one case this doesn't handle correctly is @//something@, because it's
handled differently by |Network.CGI|.

> pathList :: URI -> [String]
> pathList = splitOn "/" . decodeString . unEscapeString . uriPath

Before we actually dispatch the request, we use the opportunity to clean up the
URI and redirect the client if necessary. This handles cases like trailing
slashes. We want only one URI to point to a resource.

> dispatch' :: String -> [String] -> App CGIResult
> dispatch' method path =
>   case path of
>     ["",""]  -> frontPage -- "/"
>     ("":xs)  -> case find (== "") xs of
>                   Nothing  -> dispatch method xs
>                   Just _   -> redirect $ "/" ++ (intercalate "/" $ filter (/= "") xs)
>     _        -> output404 path

Here is where we dispatch each request to a function. We can match the request
on method and path components. This means that we can dispatch a request to one
function for a @GET@ and to another for a @POST@.

> dispatch :: String -> [String] -> App CGIResult

\subsection{Articles}

> dispatch "GET" ["article",x] = articlePage x

First, we check to see if the URL maps to an article. If so, we display it.

> dispatch "GET" path = output404 path

For now we are read-only, baby.

> dispatch _ _ = outputMethodNotAllowed ["GET"]

Finally, we get to an actual page of the site: the front page. Currently, it's
just a test of the widget system that displays the "MyLinks" widget if the
client is logged in (and nothing otherwise). It gets the common header, footer,
and associated functionality by using the stdPage function.

> frontPage :: App CGIResult
> frontPage = do
>   articles <- getArticles 10
>   let articles' = case articles of
>                     Nothing  -> return $ paragraph << "Error retrieving articles."
>                     Just as  -> map articleLinkHtml as
>   stdPage "jekor.com" [] [] [
>     thediv ! [theclass "column", identifier "left"] << [
>       boxer "Latest Articles" articles' ],
>     thediv ! [theclass "column", identifier "center"] << [
>       image ! [src "/image/word-cloud.png", alt "word (tag) cloud"] ],
>     thediv ! [theclass "column", identifier "right"] << [
>       boxer "Programs" [
>         anchor ! [href "/emacs/"] << "My Documented .emacs",
>         anchor ! [href "/gressgraph/"] << "Gressgraph: Visualize Your Firewall",
>         anchor ! [href "/xtee/"] << "Xtee: Fun with Pipes" ] ] ]

> boxer :: String -> [Html] -> Html
> boxer t ls = thediv ! [theclass "boxer"] << [
>                h2 << t,
>                unordList ls ]

%include CMS/Utils.lhs
%include CMS/CGI.lhs
%include CMS/App.lhs
%include CMS/DB.lhs
%include CMS/Html.lhs
%include CMS/Article.lhs

\end{document}
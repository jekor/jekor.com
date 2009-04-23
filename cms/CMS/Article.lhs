\section{Articles}
\label{Article}

All of Vocabulink's @www@ subdomain is served by this program. As such, if we
want to publish static data there, we need some outlet for it.

It turns out that using our program gives us some additional consistency
(standard header and footer) and abstraction.

> module CMS.Article (  articlePage, getArticle, articleBody, getArticles,
>                       articleLinkHtml ) where

> import CMS.App
> import CMS.CGI
> import CMS.DB
> import CMS.Html
> import CMS.Utils hiding ((<$$>))

> import Control.Exception (try)
> import Data.List (deleteBy)
> import Network.Gravatar (gravatarWith, size)
> import qualified System.IO.UTF8 as IO.UTF8
> import qualified Text.XHtml.Strict.Formlets as F

All articles have some common metadata.

> data Article = Article {  articleName     :: String,
>                           articleTitle    :: String,
>                           articleVersion  :: Integer,
>                           articleTime     :: UTCTime,
>                           articleFormat   :: String,
>                           articleTags     :: [String] }

Articles are actually used for articles, blog posts, and other static pages of
the site (that allow comments). The idea is that the ``blog'' section of the
site will chronologically list articles and allow interaction in a way similar
to most blogs. But there could be an ``articles'' section of the site that
contains a subset of the articles so that no time-sensitive articles are
displayed. Or an article could be used for a static page such as a
``privacy policy''.

For now, article storage is very simple. That may need to change in the future
for efficiency and to allow people without access to the article repository to
contribute.

\subsection{Retrieving Articles}

Creating articles is done outside of this program. They are currently generated
from Muse-mode (\url{http://mwolson.org/projects/EmacsMuse.html}) files using
Emacs. However, we can reconstruct the metadata for an article by parsing the
file.

Keeping the body of the article outside of the database gives us some of the
advantages of the filesystem such as revision control.

> articleDir :: App String
> articleDir = fromJust <$> getOption "articledir"

> articleBody :: Article -> App (Maybe Html)
> articleBody a = do
>   dir   <- articleDir
>   body  <- liftIO $ try $ IO.UTF8.readFile $ dir ++ "/" ++ articleName a ++ "." ++ articleFormat a
>   return $ case body of
>     Left _   -> Nothing
>     Right b  -> Just $ primHtml b

\subsection{Retrieving Articles}

Retrieve an article by its name. For now, we only support one format (HTML).

> getArticle :: String -> App (Maybe Article)
> getArticle name' = do
>   (>>= articleFromTuple) <$>
>     queryTuple' "SELECT a1.name, a1.title, a1.time, a1.format, a1.version \
>                 \FROM article_version a1 \
>                 \INNER JOIN (SELECT name, MAX(version) AS version \
>                             \FROM article_version \
>                             \WHERE name = ? \
>                             \GROUP BY name) AS a2 USING (name, version)" [toSql name']

> articleFromTuple :: [SqlValue] -> Maybe Article
> articleFromTuple [n,t,t',f,v]  = Just $
>   Article {  articleName     = fromSql n,
>              articleTitle    = fromSql t,
>              articleTime     = fromSql t',
>              articleFormat   = fromSql f,
>              articleVersion  = fromSql v,
>              articleTags     = [] }
> articleFromTuple _            = Nothing

> getArticles :: Int -> App (Maybe [Article])
> getArticles lim = do
>   rs <- queryTuples' "SELECT a1.name, a1.title, a1.time, a1.format, a1.version \
>                      \FROM article_version a1 \
>                      \INNER JOIN (SELECT name, MAX(version) AS version \
>                                  \FROM article_version \
>                                  \GROUP BY name) AS a2 USING (name, version) \
>                      \ORDER BY a1.time DESC LIMIT ?" [toSql lim]
>   case rs of
>     Nothing   -> return Nothing
>     Just rs'  -> return $ Just $ catMaybes $ map articleFromTuple rs'

\subsection{Article Pages}

Display an article to the client. We don't care whether or not it's published
(past its publication date) as unpublished articles presumably don't have links
to them.

> articlePage :: String -> App CGIResult
> articlePage name' = do
>   article <- getArticle name'
>   case article of
>     Nothing  -> output404 ["article", name']
>     Just a   -> do
>       articles <- maybe Nothing (\a' -> Just $ deleteBy (\x y -> articleName x == articleName y) a a') <$> getArticles 10
>       let articles' = case articles of
>                     Nothing  -> return $ paragraph << "Error retrieving articles."
>                     Just as  -> map articleLinkHtml $ as
>       body <- articleBody a
>       case body of
>         Nothing  -> error "Error reading article body."
>         Just b   -> do
>           r <- queryValue'  "SELECT root_comment FROM article_comments \
>                             \WHERE name = ?" [toSql name']
>           case r of
>             Just r'  -> do
>               comments <- queryTuples'
>                 "SELECT c.comment_no, t.level, c.author, \
>                        \c.time, c.comment \
>                 \FROM comment c, \
>                      \connectby('comment', 'comment_no', 'parent_no', ?, 0) \
>                      \AS t(comment_no int, parent_no int, level int) \
>                 \WHERE c.comment_no = t.comment_no" [r']
>               case comments of
>                 Nothing  -> error "Error retrieving comments."
>                 Just cs  -> do
>                   commentsHtml <- mapM (displayComment name') cs
>                   stdPage (articleTitle a) [] []
>                     [  thediv ! [theclass "sidebar"] << [
>                          h2 << "Latest Articles",
>                          unordList articles' ],
>                        thediv ! [theclass "article"] << [
>                          h1 << articleTitle a,
>                          b,
>                          paragraph ! [theclass "updated"] <<
>                            ("Last Updated " ++ show (articleTime a)),
>                          thediv ! [theclass "comments"] << commentsHtml] ]
>             Nothing  -> output404 ["article",name']

Create a clickable link HTML fragment for an article.

> articleLinkHtml :: Article -> Html
> articleLinkHtml a = anchor ! [href ("/article/" ++ articleName a)] <<
>                       articleTitle a

> commentBox :: Monad m => XHtmlForm m a -> XHtmlForm m a
> commentBox = plug (\xhtml -> thediv ! [theclass "comment toplevel editable"] << xhtml)

|parent| should be set to the comment number of the comment this is in reply to
(for replies) or nothing if this comment begins a new thread (or is commenting
directly on an object).

> commentForm :: Maybe String -> AppForm (String, Maybe Integer)
> commentForm parent = plug (\xhtml -> concatHtml [
>   thediv ! [theclass "speech soft"] << xhtml,
>   thediv ! [theclass "signature"] << [
>     helpButton "http://daringfireball.net/projects/markdown/basics" (Just "Formatting Help"),
>     isJust parent  ?  button << "Preview" +++ stringToHtml " " +++
>                       submit "" "Send Reply"
>                    $  submit "" "Create" ] ])
>     ((\a b -> (a, maybeRead =<< b))  <$> (F.textarea Nothing `check` ensures
>                 [  ((> 0)       . length, "Comment must not be empty."),
>                    ((<= 10000)  . length, "Comment must be 10,000 characters or shorter.") ])
>           <*> (nothingIfNull $ F.hidden parent))

> formatSimpleTime :: UTCTime -> String
> formatSimpleTime = formatTime' "%a %b %d, %Y %R"

> displayComment :: String -> [SqlValue] -> App Html
> displayComment name' [n, l, a, t, c]  = do
>   let n'  :: Integer  = fromSql n
>       l'  :: Integer  = fromSql l
>       a'  :: String   = fromSql a
>       t'  :: UTCTime  = fromSql t
>       c'  :: String   = fromSql c
>       id'             = "reply-" ++ (show n')
>   (_, xhtml) <- runForm' $ commentForm (Just $ show n')
>   let reply = concatHtml [
>                 button ! [  theclass $ "reveal " ++ id' ] << "Reply",
>                 paragraph ! [thestyle "clear: both"] << noHtml,
>                 thediv ! [  thestyle "display: none",
>                             identifier id',
>                             theclass "reply" ] << [
>                   thediv ! [theclass "comment editable"] <<
>                      form ! [method "POST"] << [  hidden "topic" name',
>                                                   xhtml ] ] ]
>   return $ thediv ! [  theclass "comment toplevel",
>                        thestyle $ "margin-left:" ++ (show $ l'*2) ++ "em" ] << [
>     paragraph ! [theclass "timestamp"] << formatSimpleTime t',
>     image ! [  width "60", height "60", theclass "avatar",
>                src $  gravatarWith (map toLower a')
>                                    Nothing (size 60) (Just "wavatar") ],
>     thediv ! [theclass "speech"] << displayCommentBody c',
>     thediv ! [theclass "reply-options"] << reply ]
> displayComment _ _             = return $ paragraph << "Error retrieving comment."

We don't want comment display going out-of-sync with comment previewing.

> displayCommentBody :: String -> Html
> displayCommentBody = markdownToHtml

This returns the new comment number.

> storeComment :: String -> String -> Maybe Integer -> App (Maybe Integer)
> storeComment email body parent = do
>   c <- asks appDB
>   liftIO $ insertNo c  "INSERT INTO comment (author, comment, parent_no) \
>                        \VALUES (?, ?, ?)" [toSql email, toSql body, toSql parent]
>                        "comment_comment_no_seq"

> replyToComment :: App CGIResult
> replyToComment = do
>   name'   <- getRequiredInput "name"
>   email   <- getRequiredInput "email"
>   parent  <- getInput "parent"
>   res <- runForm (commentForm parent) $ Right noHtml
>   case res of
>     Left xhtml            -> outputJSON [  ("html", showHtmlFragment $ thediv ! [theclass "comment editable"] << xhtml),
>                                            ("status", "incomplete") ]
>     Right (body,parent')  -> do
>       res' <- withTransaction' $ do
>         commentNo <- storeComment email body parent'
>         res'' <- queryTuple'  "SELECT c.comment_no, 0 as level, \
>                                      \c.author, c.time, c.comment \
>                               \FROM comment c, member m \
>                               \WHERE comment_no = ?"
>                               [toSql commentNo]
>         liftIO . commit =<< asks appDB
>         return $ fromJust res''
>       case res' of
>         Nothing  -> outputJSON [  ("html", "Error posting comment."),
>                                   ("status", "error") ]
>         Just c   -> do
>           c' <- asks appDB
>           liftIO $ commit c'
>           comment <- displayComment name' c
>           outputJSON [  ("html", showHtmlFragment comment),
>                         ("status", "accepted") ]

We need to make sure that this doesn't go out of sync with displayComment.

Does this lead to XSS vulnerabilities?

> commentPreview :: App CGIResult
> commentPreview = do
>   comment <- getRequiredInput "comment"
>   outputJSON [  ("html", showHtmlFragment $ displayCommentBody comment),
>                 ("status", "OK") ]

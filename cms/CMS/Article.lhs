\section{Articles}
\label{Article}

All of Vocabulink's @www@ subdomain is served by this program. As such, if we
want to publish static data there, we need some outlet for it.

It turns out that using our program gives us some additional consistency
(standard header and footer) and abstraction.

> module CMS.Article (  articlePage, getArticle, articleBody, getArticles,
>                       articleLinkHtml, replyToComment, commentPreview ) where

> import CMS.App
> import CMS.CGI
> import CMS.Html
> import CMS.Utils hiding ((<$$>))

> import Control.Exception (try)
> import Data.List (deleteBy, findIndices)
> import Data.Time.Clock (getCurrentTime)
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
>   body  <- liftIO (try (IO.UTF8.readFile $ dir ++ "/" ++ articleName a ++ "." ++ articleFormat a) :: IO (Either SomeException String))
>   return $ case body of
>     Left _   -> Nothing
>     Right b  -> Just $ primHtml b

\subsection{Retrieving Articles}

Retrieve an article by its name. For now, we only support one format (HTML).

> getArticle :: String -> App (Maybe Article)
> getArticle name' = liftM articleFromTuple <$> $(queryTuple'
>    "SELECT a1.name, a1.title, a1.time, a1.format, a1.version \
>    \FROM article_version a1 \
>    \INNER JOIN (SELECT name, MAX(version) AS version \
>                \FROM article_version \
>                \WHERE name = {name'} \
>                \GROUP BY name) AS a2 USING (name, version)")

> articleFromTuple :: (String, String, UTCTime, String, Integer) -> Article
> articleFromTuple (n, t, t', f, v) = Article { articleName    = n
>                                             , articleTitle   = t
>                                             , articleTime    = t'
>                                             , articleFormat  = f
>                                             , articleVersion = v
>                                             , articleTags    = []
>                                             }

> getArticles :: Int -> App [Article]
> getArticles lim = map articleFromTuple <$> $(queryTuples'
>   "SELECT a1.name, a1.title, a1.time, a1.format, a1.version \
>   \FROM article_version a1 \
>   \INNER JOIN (SELECT name, MAX(version) AS version \
>               \FROM article_version \
>               \GROUP BY name) AS a2 USING (name, version) \
>   \ORDER BY a1.time DESC LIMIT {lim}")

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
>       articles <- getArticles 10
>       let  as = cycle articles
>            i = findIndices (\x -> articleName x == articleName a) as
>            i' = i !! 2
>            prevArticle = articleLinkHtml $ as !! (i' - 1)
>            nextArticle = articleLinkHtml $ as !! (i' + 1)
>            articles'' = deleteBy (\x y -> articleName x == articleName y) a articles
>            articles' = map articleLinkHtml articles''
>       body <- articleBody a
>       case body of
>         Nothing  -> error "Error reading article body."
>         Just b   -> do
>           r <- $(queryTuple' "SELECT root_comment FROM article_comments \
>                              \WHERE name = {name'}")
>           case r of
>             Just r' -> do
>               comments <- getComments r'
>               case comments of
>                 [] -> output404 ["article", name']
>                 _  -> do
>                   commentsHtml <- mapM displayCommentWithReply $ tail comments
>                   (_, xhtml) <- runForm' $ commentForm $ r'
>                   let comment' = thediv ! [theclass "reply"] << [
>                                    thediv ! [theclass "comment editable toplevel"] <<
>                                    form ! [method "POST"] << xhtml ]
>                       commentCount = max 0 (length comments - 1)
>                       commentLink = anchor ! [href "#comments"] << (show commentCount ++ " comment" ++ (commentCount == 1 ? "" $ "s"))
>                   stdPage (articleTitle a) [  JS "MochiKit", JS "comment",
>                                               CSS "comment" ] []
>                     [  thediv ! [theclass "sidebar"] << [
>                          h2 << "Latest Articles",
>                          unordList articles' ],
>                        thediv ! [theclass "article"] << [
>                          h1 << articleTitle a,
>                          b,
>                          paragraph ! [theclass "updated"] <<
>                            ("Last Updated " ++ show (articleTime a)) ],
>                          paragraph ! [thestyle "clear: both; text-align: center"] << commentLink,
>                          thediv ! [identifier "comments", theclass "comments"] << [
>                            concatHtml commentsHtml,
>                            comment' ] ]
>                 _                    -> error "Error retrieving comments."
>             Nothing  -> output404 ["article",name']

> getComments :: Integer -> App [Comment]
> getComments root = map commentFromValues <$> $(queryTuples'
>   "SELECT * FROM comment_tree({root})")

Create a clickable link HTML fragment for an article.

> articleLinkHtml :: Article -> Html
> articleLinkHtml a = anchor ! [href ("/article/" ++ articleName a)] <<
>                       articleTitle a

|parent| should be set to the comment number of the comment this is in reply to
(for replies) or nothing if this comment begins a new thread (or is commenting
directly on an object).

> data Comment = Comment {  commentNumber    :: Integer,
>                           commentLevel     :: Integer,
>                           commentParent    :: Integer,
>                           commentRealName  :: Maybe String,
>                           commentEmail     :: Maybe String,
>                           commentURL       :: Maybe String,
>                           commentTime      :: UTCTime,
>                           commentBody      :: String }

> commentForm :: Integer -> AppForm Comment
> commentForm parent = plug (\xhtml -> concatHtml [
>   thediv ! [theclass "speech soft"] << table << tbody << xhtml,
>   thediv ! [theclass "controls"] << [
>     helpButton "http://daringfireball.net/projects/markdown/basics" (Just "Formatting Help"),
>     button << "Preview" +++ stringToHtml " " +++ submit "" "Send" ] ])
>     (mkComment  <$>  F.hidden (Just (show parent))
>                 <*>  nothingIfNull realNameForm
>                 <*>  nothingIfNull emailForm
>                 <*>  nothingIfNull urlForm
>                 <*>  commentBodyForm)
>     where mkComment p n e u b = Comment {  commentNumber    = undefined,
>                                            commentLevel     = undefined,
>                                            commentParent    = read p,
>                                            commentRealName  = n,
>                                            commentEmail     = e,
>                                            commentURL       = u,
>                                            commentTime      = undefined,
>                                            commentBody      = b }

> realNameForm :: AppForm String
> realNameForm = (plug (tabularInputHint "Real Name" "(optional)") (F.input Nothing)) `check` ensures
>   [  ((<= 100) . length, "Your Real Name must be 100 characters or shorter.") ]

> emailForm :: AppForm String
> emailForm = (plug (tabularInputHint "Email" "(optional)") (F.input Nothing)) `check` ensures
>   [  ((<= 320) . length, "Your email address must be 320 characters or shorter.") ]

> urlForm :: AppForm String
> urlForm = (plug (tabularInputHint "URL" "(optional)") (F.input Nothing)) `check` ensures
>   [  ((<= 2048) . length, "Your URL must be 2,048 characters or shorter.") ]

> commentBodyForm :: AppForm String
> commentBodyForm = plug (tabularInput "Comment") $
>   F.textarea Nothing Nothing Nothing `check` ensures
>     [  ((> 0)       . length, "Comment must not be empty."),
>        ((<= 10000)  . length, "Comment must be 10,000 characters or shorter.") ]

> formatSimpleTime :: UTCTime -> String
> formatSimpleTime = formatTime' "%a %b %d, %Y %R"

> commentFromValues :: (Maybe Integer, Maybe Integer, Maybe Integer,
>                       Maybe String, Maybe String, Maybe String, Maybe UTCTime, Maybe String) -> Comment
> commentFromValues (n, l, p, r, e, u, t, b) = Comment { commentNumber   = fromJust n
>                                                      , commentLevel    = fromJust l
>                                                      , commentParent   = fromJust p
>                                                      , commentRealName = r
>                                                      , commentEmail    = e
>                                                      , commentURL      = u
>                                                      , commentTime     = fromJust t
>                                                      , commentBody     = fromJust b
>                                                      }

> displayComment :: Comment -> Html
> displayComment c = concatHtml [
>     paragraph ! [theclass "timestamp"] << formatSimpleTime (commentTime c),
>     case commentEmail c of
>       Nothing  -> noHtml
>       Just e   -> image ! [  width "60", height "60", theclass "avatar",
>                              src $  gravatarWith (map toLower e)
>                                                  Nothing (size 60) (Just "identicon") ],
>     thediv ! [theclass "speech"] << (displayCommentBody $ commentBody c),
>     case commentRealName c of
>       Nothing  -> noHtml
>       Just n   -> paragraph ! [theclass "signature"] << [
>         let a = case commentURL c of
>                   Nothing  -> thespan
>                   Just u   -> anchor ! [href u, rel "nofollow"] in
>         a << ("—" ++ n) ] ]

> displayCommentWithReply :: Comment -> App Html
> displayCommentWithReply c = do
>   (_, xhtml) <- runForm' $ commentForm $ commentNumber c
>   let id' = "reply-" ++ (show $ commentNumber c)
>       reply = concatHtml [
>                 button ! [  theclass $ "reveal " ++ id' ] << "Reply",
>                 thediv ! [  thestyle "display: none",
>                             identifier id',
>                             theclass "reply" ] << [
>                   thediv ! [theclass "comment editable"] <<
>                      form ! [method "POST"] << xhtml ] ]
>   return $ thediv ! [  theclass "comment toplevel",
>                        thestyle $ "margin-left:" ++ (show $ (max (0 :: Double) (fromIntegral (commentLevel c - 1)) * 1.3)) ++ "em" ] << [
>     displayComment c,
>     thediv ! [theclass "controls"] << reply ]

We don't want comment display going out-of-sync with comment previewing.

> displayCommentBody :: String -> Html
> displayCommentBody = markdownToHtml

This returns the new comment number.

> storeComment :: Comment -> App (Maybe Integer)
> storeComment c =
>   case (commentRealName c, commentEmail c, commentURL c) of
>     (Just r, Just e, Just u) -> $(queryTuple'
>       "INSERT INTO comment (parent_no, name, email, url, body) \
>       \VALUES ({commentParent c}, {r}, {e}, {u}, {commentBody c}) \
>       \RETURNING comment_no")
>     (Just r, Just e, Nothing) -> $(queryTuple'
>       "INSERT INTO comment (parent_no, name, email, body) \
>       \VALUES ({commentParent c}, {r}, {e}, {commentBody c}) \
>       \RETURNING comment_no")
>     (Just r, Nothing, Just u) -> $(queryTuple'
>       "INSERT INTO comment (parent_no, name, url, body) \
>       \VALUES ({commentParent c}, {r}, {u}, {commentBody c}) \
>       \RETURNING comment_no")
>     (Nothing, Just e, Just u) -> $(queryTuple'
>       "INSERT INTO comment (parent_no, email, url, body) \
>       \VALUES ({commentParent c}, {e}, {u}, {commentBody c}) \
>       \RETURNING comment_no")
>     (Just r, Nothing, Nothing) -> $(queryTuple'
>       "INSERT INTO comment (parent_no, name, body) \
>       \VALUES ({commentParent c}, {r}, {commentBody c}) \
>       \RETURNING comment_no")
>     (Nothing, Just e, Nothing) -> $(queryTuple'
>       "INSERT INTO comment (parent_no, email, body) \
>       \VALUES ({commentParent c}, {e}, {commentBody c}) \
>       \RETURNING comment_no")
>     (Nothing, Nothing, Just u) -> $(queryTuple'
>       "INSERT INTO comment (parent_no, url, body) \
>       \VALUES ({commentParent c}, {u}, {commentBody c}) \
>       \RETURNING comment_no")
>     (Nothing, Nothing, Nothing) -> $(queryTuple'
>       "INSERT INTO comment (parent_no, body) \
>       \VALUES ({commentParent c}, {commentBody c}) \
>       \RETURNING comment_no")

> replyToComment :: App CGIResult
> replyToComment = do
>   res <- runForm (commentForm 0) $ Right noHtml
>   case res of
>     Left xhtml     -> outputJSON [  ("html", showHtmlFragment $ thediv ! [theclass "comment editable"] << xhtml),
>                                     ("status", "incomplete") ]
>     Right comment  -> do
>       commentNo <- storeComment comment
>       case commentNo of
>         Nothing -> outputJSON [  ("html", "Error posting comment."),
>                                  ("status", "error") ]
>         Just n  -> do
>           cs <- getComments n
> --      res' <- $(queryTuple' "SELECT COALESCE(c.comment_no), 0 as level, parent_no, \
> --                                   \c.name, c.email, c.url, \
> --                                   \COALESCE(c.time), COALESCE(c.body) \
> --                            \FROM comment c \
> --                            \WHERE comment_no = {commentNo}")
>           case cs of
>             [] -> outputJSON [  ("html", "Error posting comment."),
>                                 ("status", "error") ]
>             _  -> do
>               comment' <- displayCommentWithReply $ head cs
>               outputJSON [  ("html", showHtmlFragment comment'),
>                             ("status", "accepted") ]

We need to make sure that this doesn't go out of sync with displayComment.

Does this lead to XSS vulnerabilities?

> commentPreview :: App CGIResult
> commentPreview = do
>   res <- runForm (commentForm 0) $ Right noHtml
>   case res of
>     Left xhtml     -> outputJSON [  ("html", showHtmlFragment $ thediv ! [theclass "comment editable"] << xhtml),
>                                     ("status", "incomplete") ]
>     Right comment  -> do
>       t <- liftIO getCurrentTime
>       outputJSON [  ("html", showHtmlFragment $ displayComment (comment {commentTime = t})),
>                     ("status", "OK") ]

\section{Html}

Much of Vocabulink consists of simple, program-generated HTML. Rather than use
templates or HTML in strings, we use an HTML combinator library
(Text.XHtml.Strict). This makes it almost certain that our HTML will be well
formed (although we have no guarantee that it will be valid). But more
importantly, it allows us to use abstraction to get higher-level HTML-based
functions. An example of this is |linkList|.

> module CMS.Html (  Dependency(..), stdPage, simplePage,
>                           linkList, breadcrumbs, options, tableRows, accesskey,
>                           helpButton, markdownToHtml,
>                           AppForm, runForm, runForm', formLabel, formLabel',
>                           checkbox', tabularInput, tabularSubmit, tabularInputHint,
>                           pager, currentPage, copyrightStatement,
>  {- Text.XHtml.Strict -}  Html, noHtml, primHtml, stringToHtml, concatHtml,
>                           (<<), (+++), (!), showHtmlFragment,
>                           identifier, theclass, thediv, thespan, style, rel,
>                           paragraph, pre, h1, h2, h3, br, anchor, href, script,
>                           image, alt, unordList, form, action, method, enctype,
>                           hidden, label, textfield, password, button, submit,
>                           fieldset, legend, afile, textarea, select, widget,
>                           thestyle, src, width, height, value, name,
>                           cols, rows, colspan, caption,
>                           table, thead, tbody, tfoot, th, tr, td,
>  {- Text.Formlets -}      runFormState, nothingIfNull,
>                           check, ensure, ensures, checkM, ensureM,
>                           plug,
>  {- Text.XHtml.Strict.Formlets -} XHtmlForm) where

> import CMS.App
> import CMS.CGI
> import CMS.Utils

> import Control.Arrow (second)
> import Data.List (intersperse, find)
> import Text.Regex (mkRegex, subRegex)
> import Text.Regex.Posix ((=~))
> import Text.Formlets (  runFormState, plug, nothingIfNull,
>                         check, ensure, ensures, checkM, ensureM)
> import Text.Pandoc (  readMarkdown, writeHtml, defaultParserState,
>                       defaultWriterOptions )
> import Text.Formlets as F
> import Text.XHtml.Strict
> import Text.XHtml.Strict.Formlets (XHtmlForm)

Most pages depend on some external CSS and/or JavaScript files.

> data Dependency = CSS String | JS String

|stdPage| takes a title, a list of dependencies, and list of HTML objects to
place into the body of the page. It automatically adds a standard header and
footer. It also includes @page.css@.

|stdPage| expects title to already be encoded as UTF-8.

> stdPage :: String -> [Dependency] -> [Html] -> [Html] -> App CGIResult
> stdPage t deps head' body' = do
>   headerB  <- headerBar
>   footerB  <- footerBar
>   setHeader "Content-Type" "text/html; charset=utf-8"
>   output' True $ renderHtml $ header <<
>     [  thetitle << t,
>        concatHtml (map includeDep ([CSS "page"] ++ deps)),
>        thelink ! [href "/rss", thetype "application/rss+xml", rel "alternate", title "Site-wide RSS Feed"] << noHtml,
>        concatHtml head' ] +++
>     body << [  headerB,
>                jsNotice,
>                thediv ! [identifier "body"] << concatHtml body',
>                footerB ]
>  where jsNotice = case find (\e -> case e of
>                                      JS _  -> True
>                                      _     -> False) deps of
>                     Nothing  -> noHtml
>                     Just _   -> noscript << paragraph <<
>                       "This page requires JavaScript for some functionality."

Often we want a simple page where the title and header are the same.

> simplePage :: String -> [Dependency] -> [Html] -> App CGIResult
> simplePage t deps h = stdPage t deps [] $ [ h1 << t ] ++ h

Each dependency is expressed as the path from the root of the static subdomain
(for now, @s.vocabulink.com@) to the file. Do not include the file suffix
(@.css@ or @.js@), it will be appended automatically. These are meant for
inclusion in the @<head>@ of the page.

> includeDep :: Dependency -> Html
> includeDep (CSS css) =
>   thelink ! [href ("http://jekor.com/css/" ++ css ++ ".css"),
>              rel "stylesheet", thetype "text/css"] << noHtml
> includeDep (JS js) =
>   script ! [src ("http://jekor.com/js/" ++ js ++ ".js"),
>             thetype "text/javascript"] << noHtml

The standard header bar shows the Vocabulink logo (with a link to the root
page), a list of links (currently static, but eventually configurable by the
member), and either a login box and sign up button or a count of links waiting
for review and a logout button.

> headerBar :: App Html
> headerBar = do
>   return $ thediv ! [identifier "header-bar"] <<
>     [  anchor ! [theclass "logo", href "/", accesskey "1"] << [
>          stringToHtml "jekor.com", br,
>          thespan ! [theclass "tagline"] << "Programming Philosophy" ],
>        topLinks,
>        thediv ! [theclass "clear"] << noHtml ]

Here are the links we want in the header of every page.

> topLinks :: Html
> topLinks = linkList [noHtml]

The footer bar is more simple. It just includes some links to static content.

> footerBar :: App Html
> footerBar = do
>   copy <- copyrightNotice
>   return $ thediv ! [identifier "footer-bar"] <<
>     [  linkList
>        [  anchor ! [href "/rss"] << "RSS" ],
>        copy,
>        googleAnalyticsTag ]

We want a copyright notice at the bottom of every page. Since this is a
copyright notice for dynamic content, we want it to be up-to-date with the
generation time (now).

> copyrightNotice :: App Html
> copyrightNotice = do
>   year <- liftIO currentYear
>   return $ paragraph ! [theclass "copyright"] <<
>     [  stringToHtml "Copyright 2008–",
>        stringToHtml ((show year) ++ " "),
>        anchor ! [href "mailto:jekor@jekor.com"] << "Chris Forno" ]

> copyrightStatement :: App String
> copyrightStatement = do
>   year <- liftIO currentYear
>   return $ "© 2008–" ++ (show year) ++ " Chris Forno"

> googleAnalyticsTag :: Html
> googleAnalyticsTag = primHtml $ unlines [
>   "<script type=\"text/javascript\">",
>   "var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\");",
>   "document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));",
>   "</script>",
>   "<script type=\"text/javascript\">",
>   "try {",
>   "var pageTracker = _gat._getTracker(\"UA-73938-3\");",
>   "pageTracker._trackPageview();",
>   "} catch(err) {}</script>" ]

\subsection{Higher-Level Combinators}

It's common to use an unordered list to present a series of links. For example,
both the standard header and footer use this.

> linkList :: (HTML a) => [a] -> Html
> linkList items = ulist ! [theclass "hyperlinks"] << map (li <<) items

Breadcrumbs are a common navigation element. This only handles wrapping the
provided elements in an appropriate ordered list and adding decorating it.
Adding the links is up to you.

> breadcrumbs :: [Html] -> Html
> breadcrumbs items = ulist ! [theclass "breadcrumbs"] << map (li <<) items'
>   where items' = intersperse (stringToHtml " » ") items

Sometimes you just want a select list where the displayed options match their values.

> options :: [String] -> [Html]
> options choices = [ option ! [value choice] << choice | choice <- choices ]

This automatically adds the ``odd'' and ``even'' CSS classes to each table row.

> tableRows :: [Html] -> [Html]
> tableRows = map decorate . zip [1..]
>   where decorate (a,b) = tr ! [theclass (odd (a :: Integer) ? "odd" $ "even")] << b

foldr (\a ~(x,y) -> (a:y,x)) ([],[]) (fs' ++ creator)

Curiously, the accesskey attribute is missing from Text.XHtml.

> accesskey :: String -> HtmlAttr
> accesskey = strAttr "accesskey"

It's nice to have little help buttons and such where necessary. Making them
easier to create means that we're more likely to do so, which leads to a more
helpful user interface.

Currently this uses an icon from the FamFamFam "Mini" set
(http://www.famfamfam.com/lab/icons/mini/).

> helpButton :: String -> Maybe String -> Html
> helpButton url label' = anchor ! [href url, theclass "button"] << [
>                           image ! [src "http://jekor.com/image/icon_info.gif"],
>                           maybe noHtml (\x -> stringToHtml $ " " ++ x) label' ]

\subsection{Other Markup}

A modified version of Markdown (Pandoc Markdown) is used in comments and link
bodies.

> markdownToHtml :: String -> Html
> markdownToHtml = (writeHtml defaultWriterOptions) .
>                    readMarkdown defaultParserState

\subsection{Form Builders}

For complex forms, we use tables. They have a number of common elements that we
can abstract out.

One thing that's missing is the ability to link the label to the input with the
``for'' attribute.

> tabularInput :: String -> Html -> Html
> tabularInput l i = tr << [  th << (label << (l ++ ":")),
>                             td << i ]

> tabularInputHint :: String -> String -> Html -> Html
> tabularInputHint l h i = tr << [  th << (label << (l ++ ":")),
>                                   td << [i, stringToHtml (" " ++ h)] ]

> tabularSubmit :: String -> Html
> tabularSubmit l = tr << td ! [colspan 2] << submit "" l

\subsection{Formlet Helpers}

> type AppForm a = XHtmlForm (AppT IO) a

We ofter want to "wrap" a label around a form component. This doesn't currently
set a @for@ attribute.

> formLabel :: Monad m => String -> XHtmlForm m a -> XHtmlForm m a
> formLabel text = plug (\xhtml -> label << (text ++ ": ") +++ xhtml)

Here's an alterate version of the above which also adds a paragraph.

> formLabel' :: Monad m => String -> XHtmlForm m a -> XHtmlForm m a
> formLabel' text = plug (\xhtml -> paragraph << (label << (text ++ ": ") +++ xhtml))

Curiously, the formlets library is missing a checkbox implementation. Thanks to
Chris Done (http://chrisdone.com/blog/html/2008-12-14-haskell-formlets-composable-web-form-construction-and-validation.html)
for this one.

> checkbox' :: Monad m => String -> XHtmlForm m (Maybe String)
> checkbox' l = optionalInput box where
>   box name' =  input ! [thetype "checkbox", name name'] +++ l

Take a form and a submit button label, run it, and return either the form to
display (with errors, if any) or the result of the form.

``Running'' the form involves taking the form inputs from the ``environment''
(the CGI input variables) and ``passing'' them to the form. The form then
attempts to validate against the environment. If it fails, it returns a form
(as Html) to display to the client, but if it succeeds it returns a value of
the type of the form.

|s| is either a label or custom Html for the submit button (or noHtml if you
don't want a submit button).

> runForm :: XHtmlForm (AppT IO) a -> Either String Html -> App (Either Html a)
> runForm frm s = do
>   (status, xhtml) <- runForm' frm
>   case status of
>     Failure failures  -> do
>       uri   <- requestURI
>       meth  <- requestMethod
>       let submit' = case s of
>                       Left s'  -> submit "" s'
>                       Right h  -> h
>       return $ Left $ form ! [action (uriPath uri), method "POST"] <<
>                         [  (meth == "GET" ? noHtml $ unordList failures),
>                            xhtml, submit' ]
>     Success result    -> return $ Right result

This is a slimmer wrapper around runFormState for when you want to get access
to the errors before they're packed into the returned Html. This is also handy
when implementing ``preview'' functionality for forms.

> runForm' :: XHtmlForm (AppT IO) a -> App (Failing a, Html)
> runForm' frm = do
>   env <- map (second Left) <$> getInputs
>   let (res, markup, _) = runFormState env "" frm
>   status  <- res
>   xhtml   <- markup
>   return (status, xhtml)

\subsection{Paging}

We'd like to have a consistent way of ``paging'' lists that don't fit on a
single page. This could be for search results, a set of links, articles, etc.

This reads the page query parameters and returns them along with the current
offset (as a convenience).

A reasonable default is 10 items per page. We also don't want to chew up
resources retrieving too many items, so we cap the max at 100.

Limiting the paging elements to Int bounds is necessary for the functions that
use the pager (they often need to |take| some number of tuples from a list, for
instance) and does not limit the design of our HTTP interface much, if at all.
I cannot think of an instance where we'd need to go past the 65,000th page
unless we were paging through every link in the system.

> currentPage :: App (Int, Int, Int)
> currentPage = do
>   pg  <- readInputDefault 1 "pg"
>   n'  <- readInputDefault 10 "n"
>   let  n''     = n' > 100 ? 100 $ n'
>        n       = n'' < 1 ? 1 $ n''
>        offset  = (pg - 1) * n
>   return (pg, n, offset)

This will handle the query string in the links it generates while it replaces
the @n@ (number of items per page) and @page@ (the page we're on) parameters.
We give it the page we're currently on, the number of items per page, and the
total number of items available, and it does the rest.

This doesn't actually display clickable numeric links such as you'd see on a
Google search results page. It only provides the client with ``previous'' and
``next''. This is for faster database queries.

> pager :: Int -> Int -> Int -> App Html
> pager pg n total = do
>   q'   <- getVar "QUERY_STRING"
>   uri  <- requestURI
>   let  path  = uriPath uri
>        q     = maybe "" decodeString q'
>        prev  = pageQueryString n (pg - 1) q
>        next  = pageQueryString n (pg + 1) q
>   return $ paragraph ! [theclass "pager"] << thespan ! [theclass "controls"] <<
>     [  (pg > 1 ? anchor ! [href (path ++ prev), theclass "prev"] $
>         thespan ! [theclass "prev"]) << "Previous", stringToHtml " ",
>        ((pg * n < total) ? anchor ! [href (path ++ next), theclass "next"] $
>         thespan ! [theclass "next"]) << "Next" ]

Creating the query string involves keeping the existing query string intact as
much as possible. We even want the position of the parameters to stay the same
if they're already there.

> pageQueryString :: Int -> Int -> String -> String
> pageQueryString n pg q  =
>   let q1  = q   =~ nRegex   ?  subRegex (mkRegex nRegex)   q   ("n=" ++ show n) $
>                                q   ++ ("&n=" ++ show n)
>       q2  = q1  =~ pgRegex  ?  subRegex (mkRegex pgRegex)  q1  ("pg=" ++ show pg) $
>                                q1  ++ ("&pg=" ++ show pg) in
>   "?" ++ q2
>     where nRegex   = "n=[^&]+"
>           pgRegex  = "pg=[^&]+"

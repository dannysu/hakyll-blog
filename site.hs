{-# LANGUAGE OverloadedStrings, Arrows #-}

-- OverloadedStrings enables strings to become (Pattern a) automatically:
-- Pattern.hs uses parseGlob to convert string to (Pattern a)
-- http://www.haskell.org/ghc/docs/7.2.2/html/users_guide/type-class-extensions.html

import Control.Monad (forM_)
import Control.Arrow (arr, (>>>), (***), (&&&), (>>^))
import Data.Monoid (mempty, mconcat)

import Prelude hiding (id)
import Control.Category (id)

import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Internal (preEscapedString)
import Text.Blaze.Html ((!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Char (toLower)
import Data.List (isPrefixOf, tails, findIndex)

import Text.Pandoc

import Hakyll

postsPerBlogPage :: Int
postsPerBlogPage = 2

-- Allow for reference style links in markdown
pandocWriteOptions = defaultWriterOptions {
      writerReferenceLinks = True
    , writerLiterateHaskell = True
    }

-- Main program
main :: IO ()
main = hakyll $ do

    match "templates/*" $ compile templateCompiler

    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "files/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/**" $ do
        route   idRoute
        compile compressCssCompiler

    -- Labels
    create "labels" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a label list compiler for every label
    match "label/*" $ route $ labelRoute
    metaCompile $ require_ "labels"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier "label/*" t, makePostList "Posts with label" t p)))

    -- Match all files under posts directory and its subdirectories.
    -- Turn posts into wordpress style url: year/month/date/title/index.html
    forM_ [("posts/*", "templates/post.html"), ("pages/*", "templates/page.html")] $ \(p, t) ->
        match p $ do
            route   $ wordpressRoute
            compile $ pageCompilerWith defaultHakyllParserState pandocWriteOptions
                >>> addTeaser
                >>> renderTagsField "labels" (fromCapture "label/*")
                >>> applyTemplateCompiler t
                >>> requireA "recent.markdown" (setFieldA "recent" $ arr pageBody)
                >>> applyTemplateCompiler "templates/default.html"
                >>> wordpressUrlsCompiler

    -- Build list of recent posts
    match "recent_template.html" $ route idRoute
    create "recent_template.html" $ constA mempty
        >>> requireAllA "posts/*" (id *** arr (take 10 . reverse . chronological) >>> addPostList "recent" "templates/indexpostitem.html")
        >>> applyTemplateCompiler "templates/recent_template.html"

    -- Used in the second pass to include recent posts on every page
    match "recent.markdown" $ compile pageCompiler

    -- Build index page
    forM_ ["index.markdown", "404.markdown", "search.markdown"] $ \p ->
        match p $ do
            route   $ setExtension "html"
            compile $ pageCompiler
                >>> applyTemplateCompiler "templates/page.html"
                >>> requireA "recent.markdown" (setFieldA "recent" $ arr pageBody)
                >>> applyTemplateCompiler "templates/default.html"
                >>> wordpressUrlsCompiler

    -- Build blog pages
    match "blog/page/*/index.html" $ route idRoute
    metaCompile $ requireAll_ "posts/*"
        >>> arr (chunk postsPerBlogPage . reverse . chronological)
        >>^ makeBlogPages

    -- Render RSS feed
    match  "rss.xml" $ route idRoute
    create "rss.xml" $
        requireAll_ "posts/*"
        >>> arr (take 10 . reverse . chronological)
        >>> mapCompiler (arr $ copyBodyToField "description")
        >>> mapCompiler (arr $ changeField "description" trimHeader)
        >>> mapCompiler (arr $ changeField "description" trimFooter)
        >>> mapCompiler (arr $ changeField "url" (replaceAll "/index.html" (const "/")))
        >>> renderRss feedConfiguration

  where
    tagIdentifier :: Pattern (Page String) -> String -> Identifier (Page String)
    tagIdentifier pattern = fromCapture pattern

wordpressRoute :: Routes
wordpressRoute =
    gsubRoute "posts/" (const "") `composeRoutes`
        gsubRoute "pages/" (const "") `composeRoutes`
            gsubRoute "^[0-9]{4}-[0-9]{2}-[0-9]{2}-" (map replaceWithSlash)`composeRoutes`
                gsubRoute ".markdown" (const "/index.html")
    where replaceWithSlash c = if c == '-' || c == '_'
                                   then '/'
                                   else c

labelRoute :: Routes
labelRoute =
    setExtension ".html" `composeRoutes`
    gsubRoute "." adjustLink `composeRoutes`
        gsubRoute "/" (const "") `composeRoutes`
            gsubRoute "^label" (const "label/") `composeRoutes`
                gsubRoute "-html" (const "/index.html")

adjustLink = (filter (not . isSlash)) . (map (toLower . replaceWithDash))

replaceWithDash :: Char -> Char
replaceWithDash c =
    if c == '.' || c == ' '
        then '-'
        else c

isSlash :: Char -> Bool
isSlash '/' = True
isSlash _   = False

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: String -> Identifier (Template) -> Compiler (Page String, [Page String]) (Page String)
addPostList field template = setFieldA field $
    arr (reverse . chronological)
        >>> require template (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

makePostList :: String
            -> String
            -> [Page String]
            -> Compiler () (Page String)
makePostList message tag posts =
    constA (mempty, posts)
        >>> addPostList "posts" "templates/postitem.html"
        >>> arr (setField "title" (message ++ " &#8216;" ++ tag ++ "&#8217;"))
        >>> arr (setField "label" tag)
        >>> arr (setField "route" (adjustLink tag))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> requireA "recent.markdown" (setFieldA "recent" $ arr pageBody)
        >>> applyTemplateCompiler "templates/default.html"
        >>> wordpressUrlsCompiler

-- | Compiler form of 'wordpressUrls' which automatically turns index.html
-- links into just the directory name
--
wordpressUrlsCompiler :: Compiler (Page String) (Page String)
wordpressUrlsCompiler = getRoute &&& id >>^ uncurry convert
  where
    convert Nothing  = id
    convert (Just r) = fmap (wordpressUrls r)

-- | Convert URLs to WordPress style in HTML
--
wordpressUrls :: String  -- ^ Path to the site root
              -> String  -- ^ HTML to convert
              -> String  -- ^ Resulting HTML
wordpressUrls root = withUrls convert
  where
    convert x = replaceAll "/index.html" (const "/") x

-- | Split list into equal sized sublists.
-- https://github.com/ian-ross/blog
--
chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = ys : chunk n zs
    where (ys,zs) = splitAt n xs

-- | Helper function for index page metacompilation: generate
-- appropriate number of index pages with correct names and the
-- appropriate posts on each one.
-- https://github.com/ian-ross/blog
--
makeBlogPages :: [[Page String]]
              -> [(Identifier (Page String), Compiler () (Page String))]
makeBlogPages ps = map doOne (zip [1..] ps)
  where doOne (n, ps) = (indexIdentifier n, makeBlogPage n maxn ps)
        maxn = nposts `div` postsPerBlogPage +
               if (nposts `mod` postsPerBlogPage /= 0) then 1 else 0
        nposts = sum $ map length ps
        indexIdentifier n = parseIdentifier url
          where url = "blog/page/" ++ (show n) ++ "/index.html" 

-- | Make a single index page: inserts posts, sets up navigation links
-- to older and newer article index pages, applies templates.
-- https://github.com/ian-ross/blog
--
makeBlogPage :: Int -> Int -> [Page String] -> Compiler () (Page String)
makeBlogPage n maxn posts = 
    constA (mempty, posts)
    >>> addPostList "posts" "templates/teaser.html"
    >>> arr (setField "navlinkolder" (indexNavLink n 1 maxn))
    >>> arr (setField "navlinknewer" (indexNavLink n (-1) maxn))
    >>> arr (setField "title" "Danny Su")
    >>> applyTemplateCompiler "templates/blogpage.html"
    >>> requireA "recent.markdown" (setFieldA "recent" $ arr pageBody)
    >>> applyTemplateCompiler "templates/default.html"
    >>> wordpressUrlsCompiler

-- | Generate navigation link HTML for stepping between index pages.
-- https://github.com/ian-ross/blog
--
indexNavLink :: Int -> Int -> Int -> String
indexNavLink n d maxn = renderHtml ref
  where ref = if (refPage == "") then ""
              else H.a ! A.href (toValue $ toUrl $ refPage) $ 
                   (preEscapedString lab)
        lab = if (d > 0) then "Older Entries &raquo;" else "&laquo; Newer Entries"
        refPage = if (n + d < 1 || n + d > maxn) then ""
                  else case (n + d) of
                    1 -> "/blog/page/1/"
                    _ -> "/blog/page/" ++ (show $ n + d) ++ "/"

-- | RSS feed configuration.
--
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Danny Su RSS Feed"
    , feedDescription = "RSS feed for Danny Su's blog"
    , feedAuthorName  = "Danny Su"
    , feedRoot        = "http://dannysu.com"
    , feedAuthorEmail = "contact@dannysu.com"
    }

trimFooter :: String -> String
trimFooter [] = []
trimFooter xs@(x : xr)
    | "<div id=\"secondary\">" `isPrefixOf` xs = []
    | otherwise                                = x : trimFooter xr

trimHeader :: String -> String
trimHeader [] = []
trimHeader xs@(x : xr)
    | "<div id=\"primary\">" `isPrefixOf` xs = x : xr
    | otherwise                              = trimHeader xr

-- | Turns body of the page into the teaser
-- https://groups.google.com/forum/?fromgroups#!topic/hakyll/Q9wjV1Xag0c
--
addTeaser :: Compiler (Page String) (Page String)
addTeaser = arr $
    copyBodyToField "teaser"
    >>> changeField "teaser" compactTeaser
    >>> changeField "teaser" maxLengthTeaser
    >>> changeField "teaser" extractTeaser
  where
    extractTeaser :: String -> String
    extractTeaser [] = []
    extractTeaser xs@(x : xr)
        | "<!-- more -->" `isPrefixOf` xs = []
        | otherwise                       = x : extractTeaser xr

    maxLengthTeaser :: String -> String
    maxLengthTeaser s = if findIndex (isPrefixOf "<!-- more -->") (tails s) == Nothing
                            then unwords (take 60 (words s))
                            else s

    compactTeaser :: String -> String
    compactTeaser =
        (replaceAll "<img [^>]*>" (const "")) .
        (replaceAll "<p>" (const "")) .
        (replaceAll "</p>" (const "")) .
        (replaceAll "<blockquote>" (const "")) .
        (replaceAll "</blockquote>" (const "")) .
        (replaceAll "<strong>" (const "")) .
        (replaceAll "</strong>" (const "")) .
        (replaceAll "<ol>" (const "")) .
        (replaceAll "</ol>" (const "")) .
        (replaceAll "<ul>" (const "")) .
        (replaceAll "</ul>" (const "")) .
        (replaceAll "<li>" (const "")) .
        (replaceAll "</li>" (const "")) .
        (replaceAll "<h[0-9][^>]*>" (const "")) .
        (replaceAll "</h[0-9]>" (const "")) .
        (replaceAll "<pre.*" (const "")) .
        (replaceAll "<a [^>]*>" (const "")) .
        (replaceAll "</a>" (const ""))


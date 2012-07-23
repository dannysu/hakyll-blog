{-# LANGUAGE OverloadedStrings, Arrows #-}

-- OverloadedStrings enables strings to become (Pattern a) automatically:
-- Pattern.hs uses parseGlob to convert string to (Pattern a)
-- http://www.haskell.org/ghc/docs/7.2.2/html/users_guide/type-class-extensions.html

import Control.Monad (forM_)
import Control.Arrow (arr, (>>>), (***), (&&&), (>>^))
import Data.Monoid (mempty, mconcat)

import Prelude hiding (id)
import Control.Category (id)

import Data.Char (toLower)

import Text.Pandoc

import Hakyll

-- Allow for reference style links in markdown
pandocWriteOptions = defaultWriterOptions {
      writerReferenceLinks = True
    , writerLiterateHaskell = True
    }

-- Main program
main :: IO ()
main = hakyll $ do

    match "templates/*" $ compile templateCompiler

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
    match "posts/*" $ do
        route   $ wordpressRoute
        compile $ pageCompilerWith defaultHakyllParserState pandocWriteOptions
            >>> renderTagsField "labels" (fromCapture "label/*")
            >>> applyTemplateCompiler "templates/post.html"
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
    forM_ ["index.markdown", "404.markdown"] $ \p ->
        match p $ do
            route   $ setExtension "html"
            compile $ pageCompiler
                >>> applyTemplateCompiler "templates/page.html"
                >>> requireA "recent.markdown" (setFieldA "recent" $ arr pageBody)
                >>> applyTemplateCompiler "templates/default.html"
                >>> wordpressUrlsCompiler

  where
    tagIdentifier :: Pattern (Page String) -> String -> Identifier (Page String)
    tagIdentifier pattern = fromCapture pattern

wordpressRoute :: Routes
wordpressRoute =
    gsubRoute "posts/" (const "") `composeRoutes`
        gsubRoute "[0-9]{4}-[0-9]{2}-[0-9]{2}_" (map replaceWithSlash)`composeRoutes`
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

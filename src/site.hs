--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (forM_, liftM)
import           Data.Monoid ((<>))
import           Hakyll
import           Text.Pandoc (writerReferenceLinks)
import           Data.Char (toLower)
import           Data.List (isPrefixOf, tails, findIndex)

-- Allow for reference style links in markdown
pandocWriteOptions = defaultHakyllWriterOptions
    { writerReferenceLinks = True
    }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    match "templates/*" $ compile templateCompiler

    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    match "keybase.txt" $ do
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

    tags <- buildTags "posts/*" (fromCapture "label/*")

    -- Match all files under posts directory and its subdirectories.
    -- Turn posts into wordpress style url: year/month/date/title/index.html
    forM_ [("posts/*", "templates/post.html", "templates/postfooter.html"),
           ("pages/*", "templates/page.html", "templates/pagefooter.html")] $ \(p, t, f) ->
        match p $ do
            route $ wordpressRoute
            compile $ do
                let allCtx =
                        field "recent" (\_ -> recentPostList) <>
                        defaultContext

                pandocCompilerWith defaultHakyllReaderOptions pandocWriteOptions
                    >>= saveSnapshot "teaser"
                    >>= loadAndApplyTemplate t (postCtx tags)
                    >>= saveSnapshot "content"
                    >>= loadAndApplyTemplate f (postCtx tags)
                    >>= loadAndApplyTemplate "templates/default.html" allCtx
                    >>= wordpressifyUrls

    -- Build special pages
    forM_ ["index.markdown", "404.markdown", "search.markdown"] $ \p ->
        match p $ do
            route   $ setExtension "html"
            compile $ do
                let allCtx =
                        field "recent" (\_ -> recentPostList) <>
                        defaultContext

                pandocCompilerWith defaultHakyllReaderOptions pandocWriteOptions
                    >>= loadAndApplyTemplate "templates/page.html" (postCtx tags)
                    >>= loadAndApplyTemplate "templates/default.html" allCtx
                    >>= wordpressifyUrls

    -- Labels
    tagsRules tags $ \tag pattern -> do
        let title = "Posts with label " ++ " &#8216;" ++ tag ++ "&#8217;"
        route labelRoute
        compile $ do
            let allCtx =
                    field "recent" (\_ -> recentPostList) <>
                    defaultContext

            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" title <>
                            constField "posts" list <>
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" allCtx
                >>= wordpressifyUrls

    pag <- buildPaginateWith grouper "posts/*" makeId

    paginateRules pag $ \pageNum pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let paginateCtx = paginateContext pag pageNum
                ctx =
                    field "recent" (\_ -> recentPostList) <>
                    constField "title" ("Blog Archive - Page " ++ (show pageNum)) <>
                    listField "posts" (teaserCtx tags) (return posts) <>
                    paginateCtx <>
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/blogpage.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= wordpressifyUrls

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderRss feedConfiguration feedContext posts


--------------------------------------------------------------------------------
feedContext :: Context String
feedContext = mconcat
    [ rssBodyField "description"
    , rssTitleField "title"
    , wpUrlField "url"
    , dateField "date" "%B %e, %Y"
    ]

empty :: Compiler String
empty = return ""

rssTitleField :: String -> Context a
rssTitleField key = field key $ \i -> do
    value <- getMetadataField (itemIdentifier i) "title"
    let value' = liftM (replaceAll "&" (const "&amp;")) value
    maybe empty return value'

toWordPressUrl :: FilePath -> String
toWordPressUrl url =
    replaceAll "/index.html" (const "/") (toUrl url)

wpUrlField :: String -> Context a
wpUrlField key = field key $
    fmap (maybe "" toWordPressUrl) . getRoute . itemIdentifier

rssBodyField :: String -> Context String
rssBodyField key = field key $
    return .
    (replaceAll "<iframe [^>]*>" (const "")) .
    (withUrls wordpress) .
    (withUrls absolute) .
    itemBody
  where
    wordpress x = replaceAll "/index.html" (const "/") x
    absolute x = if (head x) == '/' then (feedRoot feedConfiguration) ++ x else x


--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]

teaserCtx :: Tags -> Context String
teaserCtx tags =
    Main.teaserField <>
    (postCtx tags)


--------------------------------------------------------------------------------
postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    itemTpl <- loadBody "templates/postitem.html"
    posts   <- preprocess' =<< loadAll (pattern .&&. hasNoVersion)
    applyTemplateList itemTpl (postCtx tags) posts

recentPostList :: Compiler String
recentPostList = do
    posts   <- fmap (take 10) . recentFirst =<< recentPosts
    itemTpl <- loadBody "templates/indexpostitem.html"
    list    <- applyTemplateList itemTpl defaultContext posts
    return list


--------------------------------------------------------------------------------
recentPosts :: Compiler [Item String]
recentPosts = do
    identifiers <- getMatches "posts/*"
    return [Item identifier "" | identifier <- identifiers]


--------------------------------------------------------------------------------
wordpressRoute :: Routes
wordpressRoute =
    gsubRoute "posts/" (const "") `composeRoutes`
        gsubRoute "pages/" (const "") `composeRoutes`
            gsubRoute "^[0-9]{4}-[0-9]{2}-[0-9]{2}-" (map replaceWithSlash)`composeRoutes`
                gsubRoute "\\.markdown" (const "/index.html") `composeRoutes`
                    gsubRoute "\\.rst" (const "/index.html")
    where replaceWithSlash c = if c == '-' || c == '_'
                                   then '/'
                                   else c


--------------------------------------------------------------------------------
-- | Compiler form of 'wordpressUrls' which automatically turns index.html
-- links into just the directory name
wordpressifyUrls :: Item String -> Compiler (Item String)
wordpressifyUrls item = do
    route <- getRoute $ itemIdentifier item
    return $ case route of
        Nothing -> item
        Just r  -> fmap wordpressifyUrlsWith item


--------------------------------------------------------------------------------
-- | Wordpressify URLs in HTML
wordpressifyUrlsWith :: String  -- ^ HTML to wordpressify
                     -> String  -- ^ Resulting HTML
wordpressifyUrlsWith = withUrls convert
  where
    convert x = replaceAll "/index.html" (const "/") x


--------------------------------------------------------------------------------
-- | RSS feed configuration.
--
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Danny Su"
    , feedDescription = "RSS feed for Danny Su's blog"
    , feedAuthorName  = "Danny Su"
    , feedAuthorEmail = "contact@dannysu.com"
    , feedRoot        = "http://dannysu.com"
    }


--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
teaserField :: Context String
teaserField = field "teaser" $ \item -> do
    body <- itemBody <$> loadSnapshot (itemIdentifier item) "teaser"
    return $ (maxLengthTeaser . compactTeaser) body
  where
    maxLengthTeaser :: String -> String
    maxLengthTeaser s = if findIndex (isPrefixOf "<!-- more -->") (tails s) == Nothing
                            then unwords (take 60 (words s))
                            else s
    compactTeaser :: String -> String
    compactTeaser =
        (replaceAll "<iframe [^>]*>" (const "")) .
        (replaceAll "</iframe>" (const "")) .
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
        (replaceAll "<strike>" (const "")) .
        (replaceAll "</strike>" (const "")) .
        (replaceAll "<li>" (const "")) .
        (replaceAll "</li>" (const "")) .
        (replaceAll "<div[^>]*>" (const "")) .
        (replaceAll "</div>" (const "")) .
        (replaceAll "<h[0-9][^>]*>" (const "")) .
        (replaceAll "</h[0-9]>" (const "")) .
        (replaceAll "<pre.*" (const "")) .
        (replaceAll "<a [^>]*>" (const "")) .
        (replaceAll "</a>" (const "")) .
        (replaceAll "<hr>" (const ""))


--------------------------------------------------------------------------------
-- | Pagination related functions
--
makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "blog/page/" ++ (show pageNum) ++ "/index.html"

-- Run sortRecentFirst on ids, and then liftM (paginateEvery 10) into it
grouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
grouper ids = (liftM (paginateEvery 10) . sortRecentFirst) ids

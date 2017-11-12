--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (forM_)
import           Data.Monoid ((<>))
import           Hakyll hiding (pandocCompiler)
import           WordPress
import           Feed
import           HakyllHelper
import           Pagination

--------------------------------------------------------------------------------
copyPatterns = [ "favicon.ico"
               , "keybase.txt"
               , "images/**"
               , "files/**"
               ]

main :: IO ()
main = hakyll $ do

    match "templates/*" $ compile templateCompiler

    forM_ copyPatterns $ \p ->
        match p $ do
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

                pandocCompiler
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

                pandocCompiler
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

    pag <- paginate "posts/*"

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

{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))

import Hakyll

-- Custom deploy command
hakyllConf = defaultHakyllConfiguration {
        deployCommand = "rsync -avz --delete _site/ thedumbkid@dannysu.com:~/dannysu/hakyll"
    }

-- Main program
main :: IO ()
main = hakyllWith hakyllConf $ do

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

    -- Match all files under posts directory and its subdirectories.
    -- Turn posts into wordpress style url: year/month/date/title/index.html
    match "posts/*" $ do
        route   $ wordpressRoute
        compile $ pageCompiler
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    match "index.markdown" $ do
        route   $ setExtension "html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Tags
    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)


wordpressRoute :: Routes
wordpressRoute =
    gsubRoute "posts/" (const "") `composeRoutes`
        gsubRoute "[0-9]{4}-[0-9]{2}-[0-9]{2}_" replace `composeRoutes`
            gsubRoute ".markdown" (const "/index.html")
    where replace = map replaceWithPath
                        where replaceWithPath c = 
                                  if c == '-' || c == '_'
                                      then '/'
                                      else c

{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))

import Hakyll

{- Custom deploy command -}
hakyllConf = defaultHakyllConfiguration {
        deployCommand = "rsync -avz --delete _site/ thedumbkid@dannysu.com:~/dannysu/hakyll"
    }

{- Main program -}
main :: IO ()
main = hakyllWith hakyllConf $ do

    match "templates/*" $ compile templateCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    {- Match all files under posts directory and its subdirectories. -}
    {- Turn posts into wordpress style url: year/month/date/title/index.html -}
    match "posts/**" $ do
        route   $ gsubRoute "posts/" (const "") `composeRoutes` gsubRoute ".markdown" (const "/index.html")
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    match (list ["about.rst", "index.markdown", "code.lhs"]) $ do
        route   $ setExtension "html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

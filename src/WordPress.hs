module WordPress
    ( wordpressRoute
    , wordpressifyUrls
    , wordpressifyUrlsWith
    , toWordPressUrl
    , wpUrlField
    ) where

import Hakyll

wordpressRoute :: Routes
wordpressRoute =
    gsubRoute "posts/" (const "") `composeRoutes`
        gsubRoute "pages/" (const "") `composeRoutes`
            gsubRoute "^[0-9]{4}-[0-9]{2}-[0-9]{2}-" (map replaceWithSlash) `composeRoutes`
                gsubRoute "\\.markdown" (const "/index.html")

replaceWithSlash :: Char -> Char
replaceWithSlash c = if c == '-' || c == '_'
                        then '/'
                        else c


--------------------------------------------------------------------------------
-- | Compiler form of 'wordpressUrls' which automatically turns index.html
-- links into just the directory name
wordpressifyUrls :: Item String -> Compiler (Item String)
wordpressifyUrls item = do
    r <- getRoute $ itemIdentifier item
    return $ case r of
        Nothing -> item
        Just _  -> fmap wordpressifyUrlsWith item


--------------------------------------------------------------------------------
-- | Wordpressify URLs in HTML
wordpressifyUrlsWith :: String  -- ^ HTML to wordpressify
                     -> String  -- ^ Resulting HTML
wordpressifyUrlsWith = withUrls convert
  where
    convert x = replaceAll "/index.html" (const "/") x


toWordPressUrl :: FilePath -> String
toWordPressUrl url =
    replaceAll "/index.html" (const "/") (toUrl url)


wpUrlField :: String -> Context a
wpUrlField key = field key $
    fmap (maybe "" toWordPressUrl) . getRoute . itemIdentifier

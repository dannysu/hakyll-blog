module Feed 
    ( feedConfiguration
    , feedContext
    ) where

import Control.Monad (liftM)
import Hakyll
import WordPress

--------------------------------------------------------------------------------
-- | RSS feed configuration.
--
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Danny Su"
    , feedDescription = "RSS feed for Danny Su's blog"
    , feedAuthorName  = "Danny Su"
    , feedAuthorEmail = "contact@dannysu.com"
    , feedRoot        = "https://dannysu.com"
    }

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

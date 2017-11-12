module Pagination
    ( paginate
    ) where

import Hakyll
import Control.Monad (liftM)

paginate :: MonadMetadata m => Pattern -> m Paginate
paginate pattern = buildPaginateWith grouper pattern makeId

--------------------------------------------------------------------------------
-- | Pagination related functions
--
makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "blog/page/" ++ (show pageNum) ++ "/index.html"

-- Run sortRecentFirst on ids, and then liftM (paginateEvery 10) into it
grouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
grouper ids = (liftM (paginateEvery 10) . sortRecentFirst) ids

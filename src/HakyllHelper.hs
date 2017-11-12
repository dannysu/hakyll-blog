module HakyllHelper
    ( pandocCompiler
    ) where

import Hakyll hiding (pandocCompiler)
import Text.Pandoc (writerReferenceLinks)

-- Allow for reference style links in markdown
pandocWriteOptions = defaultHakyllWriterOptions
    { writerReferenceLinks = True
    }

pandocCompiler = pandocCompilerWith defaultHakyllReaderOptions pandocWriteOptions

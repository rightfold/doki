module Doki.Wiki
( Wiki(..)

, pagePath
) where

import Doki.Page (PageID(PageID))

newtype Wiki = Wiki FilePath deriving (Show)

pagePath :: Wiki -> PageID -> FilePath
pagePath (Wiki w) (PageID p) = w ++ "/" ++ p

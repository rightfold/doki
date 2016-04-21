module Doki.Page
( PageID(..)
, PageType(..)
, pageType
) where

import System.FilePath.Posix (takeExtension)

newtype PageID = PageID FilePath deriving (Eq, Show)

newtype PageType = PageType String deriving (Eq, Ord, Show)

pageType :: PageID -> PageType
pageType (PageID id) = PageType $ drop 1 (takeExtension id)

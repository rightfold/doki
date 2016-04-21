module Doki.Page
( PageID(..)
, PageType(..)
, pageType
) where

import System.FilePath.Posix (takeExtension)

newtype PageID = PageID FilePath deriving (Eq, Show)

data PageType
  = FilePageType String
  | FolderPageType
  deriving (Eq, Ord, Show)

pageType :: PageID -> PageType
pageType (PageID id) =
  if not (null id) && last id == '/'
  then FolderPageType
  else FilePageType $ drop 1 (takeExtension id)

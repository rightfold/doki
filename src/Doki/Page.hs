module Doki.Page
( PageID(..)
, PageType(..)
, pageType
) where

import Data.List.Split (splitOn)

newtype PageID = PageID FilePath

newtype PageType = PageType String deriving (Eq, Ord, Show)

pageType :: PageID -> PageType
pageType (PageID id) =
  case splitOn "." id of
    [_] -> PageType ""
    xs -> PageType (last xs)

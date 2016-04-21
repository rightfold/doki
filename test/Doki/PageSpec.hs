module Doki.PageSpec
( spec
) where

import Doki.Page
import Test.Hspec (describe, it, shouldBe, Spec)

spec :: Spec
spec = do
  describe "pageType" $ do
    it "root" $ pageType (PageID "/") `shouldBe` FolderPageType
    it "folder" $ pageType (PageID "foo/") `shouldBe` FolderPageType
    it "no file extension" $
      pageType (PageID "namespace/main") `shouldBe` FilePageType ""
    it "file extension" $
      pageType (PageID "namespace/main.md") `shouldBe` FilePageType "md"

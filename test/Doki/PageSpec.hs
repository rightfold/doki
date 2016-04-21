module Doki.PageSpec
( spec
) where

import Doki.Page
import Test.Hspec (describe, it, shouldBe, Spec)

spec :: Spec
spec = do
  describe "pageType" $ do
    it "no file extension" $ do
      pageType (PageID "namespace/main") `shouldBe` PageType ""
    it "file extension" $ do
      pageType (PageID "namespace/main.md") `shouldBe` PageType "md"

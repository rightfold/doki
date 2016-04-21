module Doki.WikiSpec
( spec
) where

import Doki.Page (PageID(PageID))
import Doki.Wiki
import Test.Hspec (describe, it, shouldBe, Spec)

spec :: Spec
spec = do
  describe "pagePath" $ do
    it "" $ do
      pagePath (Wiki "/my/wiki") (PageID "namespace/main.md")
        `shouldBe` "/my/wiki/namespace/main.md"

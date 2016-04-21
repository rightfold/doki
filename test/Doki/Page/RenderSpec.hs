{-# LANGUAGE LambdaCase #-}
module Doki.Page.RenderSpec
( spec
) where

import Doki.Page (PageID(PageID), PageType(PageType))
import Doki.Page.Render
import Doki.Wiki (Wiki(Wiki))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (describe, it, shouldSatisfy, Spec)
import Text.XML.HaXml.Parse (xmlParse)

import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "renderPage" $ do
    it "no renderer" $ do
      go Map.empty "" $ \case
        Left NoRendererAvailable -> True
        _ -> False
    it "bad XML" $ do
      go (Map.singleton (PageType "html") (PageRenderer "cp"))
         "<strong>hello world</strong"
         $ \case
             Left (XMLParseError _) -> True
             _ -> False
    it "success" $ do
      go (Map.singleton (PageType "html") (PageRenderer "cp"))
         "<strong>hello world</strong>"
         $ \case
             Right xml -> xml == xmlParse "" "<strong>hello world</strong>"
             _ -> False
  where go renderers source predicate = do
          r <- withSystemTempDirectory "" $ \dir -> do
            writeFile (dir ++ "/main.html") source
            renderPage renderers (Wiki dir) (PageID "main.html")
          r `shouldSatisfy` predicate

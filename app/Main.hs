module Main
( main
) where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.List.Utils (replace)
import Doki.Page (PageID(PageID), PageType(PageType))
import Doki.Page.Render (PageRenderer(PageRenderer), renderPage)
import Doki.Wiki (Wiki(Wiki))
import Happstack.Server (askRq, nullConf, rqUri, setHeader, simpleHTTP, toResponse)

import qualified Data.Map as Map
import qualified Text.PrettyPrint
import qualified Text.XML.HaXml.Html.Pretty as Html.Pretty

main :: IO ()
main = do
  let wiki = Wiki "/home/rightfold/projects/doki"
  let renderers = Map.fromList [ (PageType "hs", PageRenderer "renderers/pre")
                               , (PageType "md", PageRenderer "renderers/md")
                               ]
  skin <- getSkin
  simpleHTTP nullConf $ do
    pageID <- PageID . rqUri <$> askRq
    xml' <- liftIO $ renderPage renderers wiki pageID
    return $ case xml' of
      Left err ->
        toResponse (skin "error" (show err))
        & setHeader "content-type" "text/html"
      Right xml ->
        xml
        & Html.Pretty.document
        & Text.PrettyPrint.render
        & skin (let PageID p = pageID in p)
        & toResponse
        & setHeader "content-type" "text/html"

getSkin :: IO (String -> String -> String)
getSkin = do
  html <- readFile "web/skin.html"
  return (\title body ->
    html
    & replace "<doki:title>" title
    & replace "<doki:body>" body)

module Doki.Page.Render
( RenderError(..)
, PageRenderer(..)
, PageRendererSet
, renderPage
) where

import Control.Exception (try, SomeException)
import Control.Monad.Trans.Either (EitherT(EitherT), hoistEither, runEitherT)
import Data.Bifunctor (first)
import Data.Map (Map)
import Doki.Page (PageID, PageType, pageType)
import Doki.Wiki (pagePath, Wiki)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Posn (Posn)
import Text.XML.HaXml.Types (Document)

import qualified Data.Map as Map

data RenderError
  = NoRendererAvailable
  | RendererFailure SomeException
  | XMLParseError String
  deriving (Show)

newtype PageRenderer = PageRenderer FilePath

type PageRendererSet = Map PageType PageRenderer

renderPage :: PageRendererSet
           -> Wiki
           -> PageID
           -> IO (Either RenderError (Document Posn))
renderPage rset w pid =
  case Map.lookup (pageType pid) rset of
    Nothing -> return (Left NoRendererAvailable)
    Just r -> renderPage' r (pagePath w pid)

renderPage' :: PageRenderer -> FilePath -> IO (Either RenderError (Document Posn))
renderPage' (PageRenderer r) inpath =
  withSystemTempDirectory "" $ \outdir -> runEitherT $ do
    let outpath = outdir ++ "/out"
    efrf $ try (callProcess r [inpath, outpath])
    xml <- efrf $ try (readFile outpath) -- TODO: UTF-8
    hoistEither $ XMLParseError `first` (xmlParse' outpath xml)
  where efrf a = EitherT $ first RendererFailure <$> a

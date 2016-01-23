module Main where

import           Reflex
import           Reflex.Dom
import           Reflex.Spider.Internal (SpiderHostFrame)
import qualified Data.Text.Lazy         as TL
import Data.Text.Encoding as TLE

import Leaflet
import Layout
import Expenditure
import ReflexExtensions

main :: IO ()
main = mainWidget expenditureCard

headSection :: Widget Spider (Gui Spider (WithWebView SpiderHost) SpiderHostFrame) ()
headSection = do
  metaUtf8
  metaViewport "width=device-width, initial-scale=1"
  styleInline $ TL.unpack readCss




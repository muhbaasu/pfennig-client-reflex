{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

-- | based on @ali-abrar
module Leaflet where

import Reflex.Dom
import Data.Monoid
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.DOM.Element
import GHCJS.DOM.Types
import Control.Monad.IO.Class

newtype LeafletMap = LeafletMap { unLeafletMap :: JSRef LeafletMap }

newtype LeafletTileLayer = LeafletTileLayer { unLeafletTileLayer :: JSRef LeafletTileLayer }

foreign import javascript unsafe "L['map']($1)" leafletMap_ :: JSRef Element -> IO (JSRef LeafletMap)

foreign import javascript unsafe "$1['setView']([$2, $3], $4)" leafletMapSetView_ :: JSRef LeafletMap -> Double -> Double -> Int -> IO ()

foreign import javascript unsafe "L['tileLayer']($1, { maxZoom: $2, attribution: $3})" leafletTileLayer_ :: JSString -> Int -> JSString -> IO (JSRef LeafletTileLayer)

foreign import javascript unsafe "$1['addTo']($2)" leafletTileLayerAddToMap_ :: JSRef LeafletTileLayer -> JSRef LeafletMap -> IO ()

foreign import javascript unsafe "$1['invalidateSize']()" leafletMapInvalidateSize_ :: JSRef LeafletMap -> IO ()

leafletMap :: IsElement e => e -> IO LeafletMap
leafletMap e = do
  lm <- leafletMap_ $ unElement $ toElement e
  return $ LeafletMap lm

leafletMapSetView :: LeafletMap -> (Double, Double) -> Int -> IO ()
leafletMapSetView lm (lat, lng) zoom =
  leafletMapSetView_ (unLeafletMap lm) lat lng zoom

leafletTileLayer :: String -> Int -> String -> IO LeafletTileLayer
leafletTileLayer src maxZoom attribution = do
  ltl <- leafletTileLayer_ (toJSString src) maxZoom (toJSString attribution)
  return $ LeafletTileLayer ltl

leafletTileLayerAddToMap :: LeafletTileLayer -> LeafletMap -> IO ()
leafletTileLayerAddToMap ltl lm = leafletTileLayerAddToMap_ (unLeafletTileLayer ltl) (unLeafletMap lm)

--cardMap :: IsElement e => e -> IO ()
--cardMap elementRef = do
--  lm <- do
--     lm <- leafletMap elementRef
--     leafletMapSetView lm (40.769, -73.965) 13
--     let osurl = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
--         oselem = "&copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>"
--     ltl <- leafletTileLayer osurl 15 oselem
--     leafletTileLayerAddToMap ltl lm
--     return lm
--  postBuild <- getPostBuild
--  performEvent_ $ fmap (\_ -> liftIO $ leafletMapInvalidateSize_ $ unLeafletMap lm) postBuild

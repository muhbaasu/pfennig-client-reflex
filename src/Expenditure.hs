{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Expenditure (expenditureCard) where

import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Data.Time.LocalTime    (LocalTime)
import           Control.Monad.IO.Class  (liftIO)
import           GHCJS.Foreign          ()
import           Reflex.Dom

import           Layout                 (readCss)
import           Leaflet
import           ReflexExtensions

newtype ExpenditureId = ExpenditureId Int deriving (Eq, Show)

data ExpenditureTag = ExpenditureTag {
       _tagId        :: ExpenditureTag
     , _tagTitle     :: T.Text
     , _tagCreatedAt :: LocalTime
     } deriving (Show)

-- | a circular tag
_tag :: MonadWidget t m => String -> m ()
_tag s =
  elAttr "div" ("class" =: "chip") $ do
    text s
    elAttr "i" ("class" =: "material-icons") $ text "close"

-- | font awesome icon
_faicon :: MonadWidget t m => String -> m ()
_faicon c =
  let ic = "fa fa-" <> c
  in iconClass ic ""

expenditureCard :: MonadWidget t m => m ()
expenditureCard = divClass "card large" $ do
  divClass "card-map" $ do
    (cardMap, _) <- elAttr' "div" ("style" =: "height: 240px;") $ return ()
    rowClass "expenditure-elem indigo" $ do
      elAttr "span" ("class" =: "card-title-custom flex-max") $
        text "Food purchases"
      elAttr "span" ("class" =: "card-title-custom card-title-right") $
        text "39.95$"
    lm <- liftIO $ do
      lm <- leafletMap $ _el_element cardMap
      leafletMapSetView lm (51.505, -0.09) 13
      let osurl = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
          oselem = "&copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>"
      ltl <- leafletTileLayer osurl 15 oselem
      leafletTileLayerAddToMap ltl lm
      return lm
    postBuild <- getPostBuild
    performEvent_ $ fmap (\_ -> liftIO $ leafletMapInvalidateSize_ $ unLeafletMap lm) postBuild
  divClass "card-content" $ do
    rowClass "expenditure-elem" $ do
      _faicon "clock-o flex-max"
      divClass "" $ text "15.02.2016 - 16:43:00"
    rowClass "expenditure-elem" $ do
      divClass "label flex-max" $ text "Payment method"
      _faicon "cc-visa"
    rowClass "expenditure-elem" $ do
      divClass "label flex-max" $ text "Tags"
      divClass "" $ do
        _tag "Food"
        _tag "Weekly"
    rowClass "expenditure-elem" $ do
      divClass "label flex-max" $ text "Shops"
      divClass "" $ do
        _tag "Walmart"
        _tag "Woolworth"
  divClass "card-action" $ do
    elAttr "a" ("href" =: "#") $ text "edit"
    elAttr "a" ("href" =: "#") $ text "delete"
  return ()


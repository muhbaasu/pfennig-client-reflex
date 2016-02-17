{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Expenditure (expenditureCard) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Data.Time.LocalTime    (LocalTime)
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
expenditureCard = divClass "card" $ do
  divClass "card-map" $ do
    (cardMap, _) <- elAttr' "div" ("style" =: "height: 240px;") $ return ()
    rowClass "card-title-row blue-grey darken-1" $ do
      elAttr "span" ("class" =: "card-title-custom") $
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
    elAttr "form" ("action" =: "#" <> "class" =: "flex-row expenditure-form") $ do
      divClass "input-field col" $ do
        let attrs = ("id" =: "time"
                  <> "name" =: "time"
                  <> "disabled" =: "")
        time' <- textInput $ def & textInputConfig_initialValue .~ "2018-01-03T00:01"
                                 & attributes .~ constDyn attrs
        label "Time" "time"
        return time'
      divClass "input-field col" $ do
        let attrs = ("id" =: "payment-method"
                  <> "name" =: "payment-method"
                  <> "disabled" =: "")
        time' <- textInput $ def & textInputConfig_initialValue .~ "VISA"
                                   & attributes .~ constDyn attrs
        label "Payment method" "payment-method"
      divClass "input-field col" $ do
        let attrs = ("id" =: "tags"
                  <> "name" =: "tags"
                  <> "disabled" =: "")
        time' <- textInput $ def & textInputConfig_initialValue .~ "weekly, groceries"
                                   & attributes .~ constDyn attrs
        label "Tags" "tags"
  divClass "card-action" $ do
    elAttr "a" ("href" =: "#") $ text "edit"
    elAttr "a" ("href" =: "#") $ text "delete"

  -- Workaround for Materalize CSS which forces a text update of the
  -- just rendered form inputs
  postBuild <- getPostBuild
  performEvent_ $ fmap (\_ -> liftIO $ updateTextFields_) postBuild

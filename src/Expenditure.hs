{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Expenditure
    ( someFunc
    ) where

import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           Data.Time.LocalTime    (LocalTime)
import           GHCJS.Foreign          ()
import           Reflex
import           Reflex.Dom
import           Reflex.Spider.Internal (SpiderHostFrame)

import           Layout                 (readCss)
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

--expenditureRow :: forall t m a. MonadWidget t m => m a -> m a ->  m ()
-- expenditureRow lbl content = do
--    rowClass "" $ do
--      divClass "" lbl
--      divClass "" content

someFunc :: IO ()
someFunc = mainWidgetWithHead headSection expenditureCard

headSection :: Widget Spider (Gui Spider (WithWebView SpiderHost) SpiderHostFrame) ()
headSection = do
  metaUtf8
  metaViewport "width=device-width, initial-scale=1"
  styleInline $ TL.unpack readCss
  stylesheet "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/css/materialize.min.css"
  stylesheet "https://fonts.googleapis.com/icon?family=Material+Icons"
  stylesheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
  scriptSrc "https://code.jquery.com/jquery-2.1.4.min.js"
  scriptSrc "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/js/materialize.min.js"

expenditureCard :: MonadWidget t m => m ()
expenditureCard = divClass "card large" $ do
  divClass "card-image" $ do
    elAttr "img" ("src" =: "assets/map.png") blank
    elAttr "span" ("class" =: "card-title red-text") $
      text "Food purchases"
    elAttr "span" ("class" =: "card-title-right red-text") $
      text "39.95$"
  divClass "card-content" $ do
    rowClass "expenditure-row" $ do
      rowClass "expenditure-elem" $ do
        _faicon "clock-o"
        divClass "" $ text "15.02.2016 - 16:43:00"
      rowClass "expenditure-elem" $ do
        divClass "label" $ text "Payment method"
        divClass "" $ _faicon "cc-visa"
    rowClass "expenditure-row" $ do
      rowClass "expenditure-elem" $ do
        divClass "label" $ text "Tags"
        divClass "" $ do
          _tag "Food"
          _tag "Weekly"
      rowClass "expenditure-elem" $ do
        divClass "label" $ text "Shops"
        divClass "" $ do
          _tag "Walmart"
          _tag "Woolworth"
  divClass "card-action" $ do
    elAttr "a" ("href" =: "#") $ text "edit"
    elAttr "a" ("href" =: "#") $ text "delete"
  return ()


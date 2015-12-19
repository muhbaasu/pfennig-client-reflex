{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Lib
    ( someFunc
    ) where

import Data.Text.Lazy (unpack)
import GHCJS.Foreign ()
import Reflex
import Reflex.Dom
import Control.Monad (MonadPlus(), mfilter)
import Data.Monoid ((<>))
import qualified Data.Map as Map
import Reflex.Spider.Internal(SpiderHostFrame)

import Layout (readCss)

data UserFields = UserFields {
    _usrFmail    :: String
  , _usrFPassword :: String
  } deriving (Show)

data LoginFields = LoginFields {
    _usrFields :: UserFields
  , _rememberMe :: Bool
  } deriving (Show)

mNotEmpty :: (Eq a, Monoid a, MonadPlus m) => m a -> m a
mNotEmpty = mfilter (not . (== mempty))

metaViewport :: MonadWidget t m => String -> m ()
metaViewport s = elAttr "meta" ("name" =: "viewport" <> "content" =: s) blank

stylesheet :: MonadWidget t m => String -> m ()
stylesheet s = elAttr "link" ("rel" =: "stylesheet" <> "href" =: s) blank

styleInline :: MonadWidget t m => String -> m ()
styleInline s = el "style" $ text s

scriptSrc :: MonadWidget t m => String -> m ()
scriptSrc s = elAttr "script" ("src" =: s) blank

label :: MonadWidget t m => String -> String -> m ()
label l f = labelClass l f ""

labelClass :: MonadWidget t m => String -> String -> String -> m ()
labelClass l f c = elAttr "label" ("for" =: f <> "class" =: c) $ text l

-- | s - button text, c - button class
buttonClass :: MonadWidget t m => String -> String -> m (Event t ())
buttonClass s c = do
  (e, _) <- elAttr' "button" ("class" =: c) $ text s
  return $ domEvent Click e

formClass :: MonadWidget t m => String -> m ()
formClass c = elAttr "form" ("class" =: c) blank

headSection :: Widget Spider (Gui Spider (WithWebView SpiderHost) SpiderHostFrame) ()
headSection = do
  metaViewport "width=device-width, initial-scale=1"
  styleInline $ unpack readCss
  stylesheet "https://storage.googleapis.com/code.getmdl.io/1.0.6/material.indigo-pink.min.css"
  stylesheet "https://fonts.googleapis.com/icon?family=Material+Icons"
  scriptSrc "https://storage.googleapis.com/code.getmdl.io/1.0.6/material.min.js"

someFunc :: IO ()
someFunc = mainWidgetWithHead headSection login

row :: forall t m a. MonadWidget t m => m a -> m a
row = divClass "row"

rowClass :: forall t m a. MonadWidget t m => String ->m a -> m a
rowClass c= divClass $ "row " <> c

login :: MonadWidget t m => m ()
login = divClass "center" $
  elAttr "form" ("action" =: "#" <> "class" =: "login") $ do
    elClass "h3" "Login" $ text "Login"
    z <- loginField
    x <- mapDyn show z
    row $ do
      elAttr "a" ("href" =: "/register" <> "class" =: "frm-btn mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect") $ text "Register"
      elAttr "a" ("href" =: "#" <> "class" =: "frm-btn mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect") $ text "Forgot password?"
    row $ buttonClass "Login" "frm-btn mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored"
    dynText x

loginField :: MonadWidget t m => m (Dynamic t (Maybe LoginFields))
loginField = do
  lc <- loginCredentials
  cb <- rowClass "mdl-textfield" $ do
    labelClass "" "remember" "mdl-checkbox mdl-js-checkbox mdl-js-ripple-effect"
    checkbox False $ def & attributes .~ constDyn ("id" =: "remember" <> "class" =: "mdl-checkbox__input")
  combineDyn (\x y -> LoginFields <$> x <*> Just y) lc (_checkbox_value cb)

loginCredentials :: MonadWidget t m => m (Dynamic t (Maybe UserFields))
loginCredentials = do
  user <- rowClass "mdl-textfield mdl-js-textfield mdl-textfield--floating-label" $ do
    ti <- textInput $ def & attributes .~ constDyn ("id" =: "email" <> "name" =: "email" <> "type" =: "email" <> "class" =: "mdl-textfield__input")
    labelClass "E-Mail" "email" "mdl-textfield__label"
    return $ ti
  pw <- rowClass "mdl-textfield mdl-js-textfield mdl-textfield--floating-label" $ do
    ti <- textInput $ def & attributes .~ constDyn ("id" =: "pass" <> "name" =: "pass" <> "type" =: "password" <> "class" =: "mdl-textfield__input")
    labelClass "Password" "pass" "mdl-textfield__label"
    return $ ti
  mayUser <- mapDyn (mNotEmpty . Just) $ _textInput_value user
  mayPw <- mapDyn (mNotEmpty . Just) $ _textInput_value pw
  combineDyn (\x y -> UserFields <$> x <*> y) mayUser mayPw


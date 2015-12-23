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
import Reflex.Spider.Internal(SpiderHostFrame)

import Layout           (readCss)
import ReflexExtensions

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

row :: forall t m a. MonadWidget t m => m a -> m a
row = divClass "row"

rowClass :: forall t m a. MonadWidget t m => String ->m a -> m a
rowClass c= divClass $ "row " <> c

someFunc :: IO ()
someFunc = mainWidgetWithHead headSection login

headSection :: Widget Spider (Gui Spider (WithWebView SpiderHost) SpiderHostFrame) ()
headSection = do
  metaUtf8
  metaViewport "width=device-width, initial-scale=1"
  styleInline $ unpack readCss
  stylesheet "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/css/materialize.min.css"
  stylesheet "https://fonts.googleapis.com/icon?family=Material+Icons"
  scriptSrc "https://code.jquery.com/jquery-2.1.4.min.js"
  scriptSrc "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/js/materialize.min.js"

login :: MonadWidget t m => m ()
login = divClass "center" $
  elAttr "form" ("action" =: "#" <> "class" =: "login card") $ do
    elClass "h3" "Login" $ text "Login"
    z <- loginField
    x <- mapDyn show z
    row $ do
      let regMap = ( "href" =: "/register"
                  <> "class" =: "frm-btn waves-effect waves-teal btn-flat")
          fpwMap = ( "href" =: "#"
                  <> "class" =: "frm-btn waves-effect waves-teal btn-flat")
      elAttr "a" regMap $ text "Register"
      elAttr "a" fpwMap $ text "Forgot password?"
    row $ buttonClass "Login" "frm-btn btn waves-effect waves-light"
    dynText x

loginField :: MonadWidget t m => m (Dynamic t (Maybe LoginFields))
loginField = do
  lc <- loginCredentials
  cb <- rowClass "switch" $ do
    el "label" $ do
      cb'' <- checkbox False $ def & attributes .~ constDyn ("id" =: "remember")
      elAttr "span" ("class" =: "lever") blank
      text "Remember Me"
      return cb''
  combineDyn (\x y -> LoginFields <$> x <*> Just y) lc (_checkbox_value cb)

loginCredentials :: MonadWidget t m => m (Dynamic t (Maybe UserFields))
loginCredentials = do
  user <- rowClass "input-field" $ do
    let mailMap = ("id" =: "email"
                <> "name" =: "email"
                <> "type" =: "email"
                <> "required" =: "")
    user' <- textInput $ def & attributes .~ constDyn mailMap
    label "E-Mail" "email"
    return user'
  pw <- rowClass "input-field" $ do
    let pwMap = ( "id" =: "pass"
               <> "name" =: "pass"
               <> "type" =: "password"
               <> "required" =: "")
    pw' <- textInput $ def & attributes .~ constDyn pwMap
    label "Password" "pass"
    return pw'
  mayUser <- mapDyn (mNotEmpty . Just) $ _textInput_value user
  mayPw <- mapDyn (mNotEmpty . Just) $ _textInput_value pw
  combineDyn (\x y -> UserFields <$> x <*> y) mayUser mayPw


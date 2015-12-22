{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Lib
    ( someFunc
    ) where

import Data.Text.Lazy (unpack)
import Data.ByteString.UTF8(toString)
import GHCJS.Foreign ()
import Reflex
import Reflex.Dom
import Control.Monad (MonadPlus(), mfilter)
import Data.Monoid ((<>))
import Data.FileEmbed
import qualified Data.Map as Map
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
  styleInline $ toString $ $(embedFile "assets/surface_styles.css")

login :: MonadWidget t m => m ()
login = divClass "center" $
  elAttr "form" ("action" =: "#" <> "class" =: "login card") $ do
    elClass "h3" "Login" $ text "Login"
    z <- loginField
    x <- mapDyn show z
    row $ do
      let regMap = ("href" =: "/register" <> "class" =: "frm-btn btn--flat")
          fpwMap = ("href" =: "#" <> "class" =: "frm-btn btn--flat")
      elAttr "a" regMap $ text "Register"
      elAttr "a" fpwMap $ text "Forgot password?"
    row $ buttonClass "Login" "frm-btn btn--raised btn--primary"
    dynText x

loginField :: MonadWidget t m => m (Dynamic t (Maybe LoginFields))
loginField = do
  lc <- loginCredentials
  cb <- row $ do
    cb' <- checkbox False $ def & attributes .~ constDyn ("id" =: "remember")
    elAttr "label" ("for" =: "remember") $ text "Remember me"
    return cb'
  combineDyn (\x y -> LoginFields <$> x <*> Just y) lc (_checkbox_value cb)

loginCredentials :: MonadWidget t m => m (Dynamic t (Maybe UserFields))
loginCredentials = do
  user <- row $ do
    let mailMap = ("id" =: "email"
                <> "placeholder" =: "E-Mail"
                <> "name" =: "email"
                <> "type" =: "email"
                <> "required" =: "")
    textInput $ def & attributes .~ constDyn mailMap
  pw <- row $ do
    let pwMap = ( "id" =: "pass"
               <> "placeholder" =: "Password"
               <> "name" =: "pass"
               <> "type" =: "password"
               <> "required" =: "")
    textInput $ def & attributes .~ constDyn pwMap
  mayUser <- mapDyn (mNotEmpty . Just) $ _textInput_value user
  mayPw <- mapDyn (mNotEmpty . Just) $ _textInput_value pw
  combineDyn (\x y -> UserFields <$> x <*> y) mayUser mayPw


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Login
    ( someFunc
    ) where

import           Control.Monad          (MonadPlus (), mfilter)
import           Data.Monoid            ((<>))
import           Data.Text.Lazy         (unpack)
import           GHCJS.Foreign          ()
import           Reflex
import           Reflex.Dom
import           Reflex.Spider.Internal (SpiderHostFrame)

import           Layout                 (readCss)
import           ReflexExtensions

data UserFields = UserFields {
    _usrFmail     :: String
  , _usrFPassword :: String
  } deriving (Show)

data LoginFields = LoginFields {
    _loginFmail     :: String
  , _loginFPassword :: String
  , _rememberMe     :: Bool
  } deriving (Show)

data LoginEvent = Login (Maybe LoginFields)
                | Registration UserFields

mNotEmpty :: (Eq a, Monoid a, MonadPlus m) => m a -> m a
mNotEmpty = mfilter (not . (== mempty))

someFunc :: IO ()
someFunc = mainWidgetWithHead headSection $ do
  log <- login
  l <- holdDyn Nothing log
  m <- mapDyn show l
  dynText m

headSection :: Widget Spider (Gui Spider (WithWebView SpiderHost) SpiderHostFrame) ()
headSection = do
  metaUtf8
  metaViewport "width=device-width, initial-scale=1"
  styleInline $ unpack readCss
  stylesheet "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/css/materialize.min.css"
  stylesheet "https://fonts.googleapis.com/icon?family=Material+Icons"
  scriptSrc "https://code.jquery.com/jquery-2.1.4.min.js"
  scriptSrc "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/js/materialize.min.js"

login :: MonadWidget t m => m (Event t (Maybe LoginFields))
login = divClass "center" $
  elAttr "form" ("action" =: "#" <> "class" =: "login card") $ do
    elClass "h3" "Login" $ text "Login"
    z <- loginField
    row $ do
      let regMap = ( "href" =: "/register"
                  <> "class" =: "frm-btn waves-effect waves-teal btn-flat")
          fpwMap = ( "href" =: "/lost-password"
                  <> "class" =: "frm-btn waves-effect waves-teal btn-flat")
      elAttr "a" regMap $ text "Register"
      elAttr "a" fpwMap $ text "Forgot password?"
    btn <- row $ buttonClass "Login" "frm-btn btn waves-effect waves-light"
    return $ tag (current z) btn

loginField :: MonadWidget t m => m (Dynamic t (Maybe LoginFields))
loginField = do
  lc <- loginCredentials
  cb <- rowClass "switch" $ do
    el "label" $ do
      cb'' <- checkbox False $ def & attributes .~ constDyn ("id" =: "remember")
      elAttr "span" ("class" =: "lever") blank
      text "Remember Me"
      return cb''
  let loginFn = \x y -> LoginFields <$> (_usrFmail <$> x) <*> (_usrFPassword <$> x) <*> Just y
  combineDyn loginFn lc (_checkbox_value cb)

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


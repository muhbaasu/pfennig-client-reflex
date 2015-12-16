{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Lib
    ( someFunc
    ) where

import GHCJS.Foreign ()
import Reflex
import Reflex.Dom
import Control.Monad (MonadPlus(), mfilter)
import Data.Monoid ((<>))
import qualified Data.Map as Map
import Reflex.Spider.Internal(SpiderHostFrame)

data UserFields = UserFields {
    _usrFmail    :: String
  , _usrFPassword :: String
  } deriving (Show)

data LoginFields = LoginFields {
    _usrFields :: UserFields
  , _rememberMe :: Bool
  } deriving (Show)

metaViewport :: MonadWidget t m => String -> m ()
metaViewport s = elAttr "meta" ("name" =: "viewport" <> "content" =: s) blank

stylesheet :: MonadWidget t m => String -> m ()
stylesheet s = elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", s)]) $ return ()

scriptSrc :: MonadWidget t m => String -> m ()
scriptSrc s = elAttr "script" (Map.fromList [("type", "javascript"), ("src", s)]) $ return ()

headSection :: Widget Spider (Gui Spider (WithWebView SpiderHost) SpiderHostFrame) ()
headSection = do
  metaViewport "width=device-width, initial-scale=1"
  stylesheet "https://storage.googleapis.com/code.getmdl.io/1.0.6/material.indigo-pink.min.css"
  stylesheet "https://fonts.googleapis.com/icon?family=Material+Icons"
  scriptSrc "https://storage.googleapis.com/code.getmdl.io/1.0.6/material.min.js"

someFunc :: IO ()
someFunc = mainWidgetWithHead headSection login

row :: forall t m a. MonadWidget t m => m a -> m a
row = divClass "row"

mNotEmpty :: (Eq a, Monoid a, MonadPlus m) => m a -> m a
mNotEmpty = mfilter (not . (== mempty))

login :: MonadWidget t m => m ()
login = divClass "center" $
  elClass "form" "login" $ do
    elClass "h3" "Login" $ text "Login"
    z <- loginField
    x <- mapDyn show z
    row $ do
      elAttr "a" ("href" =: "/register") $ text "Register"
      elAttr "a" ("href" =: "#") $ text "Forgot password?"
    dynText x

loginField :: MonadWidget t m => m (Dynamic t (Maybe LoginFields))
loginField = do
  lc <- loginCredentials
  -- el "label" $ text "Remember me"
  cb <- row $ checkbox False def
  combineDyn (\x y -> LoginFields <$> x <*> Just y) lc (_checkbox_value cb)

loginCredentials :: MonadWidget t m => m (Dynamic t (Maybe UserFields))
loginCredentials = do
  user <- row $ textInput def
  pw <- row $ textInput def
  mayUser <- mapDyn (mNotEmpty . Just) $ _textInput_value user
  mayPw <- mapDyn (mNotEmpty . Just) $ _textInput_value pw
  combineDyn (\x y -> UserFields <$> x <*> y) mayUser mayPw

   --el "div" $ do
  ----  elAttr "div" ("class" =: "center")
  ----  return ()

--    form_ [class_ "login", method_ "post", action_ "/login"] $ do
--      h3_ "Login"
--     el "div" $ [class_ "row"] $
--        input_ [id_ "email", name_ "email",
--                placeholder_ "E-Mail",
--                type_ "text"]
--     el "div" $ [class_ "row"] $
--        input_ [id_ "pass", name_ "pass",
--                placeholder_ "Password",
--                type_ "password"]
--     el "div" $ [class_ "row"] $ do
--        label_ [for_ "remember"] "Remember me"
--        input_ [id_ "remember",
--                type_ "checkbox",
--                name_ "remember",
--                value_ "remember"]
--     el "div" $ [class_ "row"] $ do
--        a_ [href_ "/register"] "Register"
--        a_ [href_ "#"] "Forgot password?"
--     el "div" $ [class_ "row"] $
--        button_ [type_ "submit",
--                 value_ "submit"] "Login"

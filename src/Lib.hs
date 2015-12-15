{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Lib
    ( someFunc
    ) where

import GHCJS.Foreign ()
import GHCJS.Types
import Reflex
import Reflex.Dom
import Data.FileEmbed
import Safe(readMay)

--import Layout (readCSS)

data UserFields = UserFields {
    _usrFmail    :: String
  , _usrFPassword :: String
  } deriving (Show)

data LoginFields = LoginFields {
    _usrFields :: UserFields
  , _rememberMe :: Bool
  } deriving (Show)

someFunc :: IO ()
someFunc = mainWidgetWithCss $(embedFile "assets/style.css") login

row :: forall t m a. MonadWidget t m => m a -> m a
row = divClass "row"

login :: MonadWidget t m => m ()
login = divClass "center" $
  elClass "form" "login" $ do
    elClass "h3" "Login" $ text "Login"
    z <- loginField
    x <- mapDyn show z
    dynText x

--    row $ do
--      link "/register" $ text "Register"
--      link "#" $ text "Forgot password?"

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
  mayUser <- mapDyn readMay $ _textInput_value user
  mayPw <- mapDyn readMay $ _textInput_value pw
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

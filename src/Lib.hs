{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( someFunc
    ) where

import GHCJS.Foreign ()
import GHCJS.Types
import Reflex
import Reflex.Dom
import Data.FileEmbed

--import Layout (readCSS)

someFunc :: IO ()
someFunc = mainWidgetWithCss $(embedFile "assets/style.css") login

login :: MonadWidget t m => m ()
login = divClass "center" $
  elClass "form" "login" $ text "bla"
 --do
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

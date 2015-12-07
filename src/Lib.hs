{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import GHCJS.Foreign ()
import GHCJS.Types
import Reflex.Dom

someFunc = mainWidget $ el "div" $ text "Welcome to Reflex"

{-# LANGUAGE OverloadedStrings #-}

module Layout where

import           Clay                    hiding (wrap, (**))
import           Clay.Flexbox            (wrap)
import           Data.ByteString.Lazy    (ByteString)
import           Data.Text.Lazy          (Text)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy.IO       as LIO
import           Prelude                 hiding (rem)

renderCss :: IO ()
renderCss = do
  let css = render pfennig
  LIO.writeFile "./assets/generated.css" css

readCss :: Text
readCss = render pfennig

pfennig :: Css
pfennig = do
  html ? do
    boxSizing borderBox

  star ?
    boxSizing inherit

  base'

  ".title" ? do
    fontFamily ["comfortaa"] [sansSerif]
    fontFace $ importUrl "assets/comfortaa-regular-webfont.woff"
    fontSize (rem 1.4)

base' :: Css
base' = do
  ".row" ? do
    display flex
    alignItems center
    flexDirection row

  ".column" ? do
    display flex
    flexDirection column

  ".center" ? do
    display flex
    height $ pct 100
    width $ pct 100
    justifyContent center
    alignItems center

  ".frm-btn"? do
    flexGrow 1
    margin (vmin 0.6) (vmin 0.6) (vmin 0.6) (vmin 0.6)

  ".expenditure-elem" ? do
    justifyContent spaceBetween
    flexGrow 1

  ".expenditure-row" ? do
    flexWrap wrap

  ".card-title-row" ? do 
    backgroundColor "#fff"

  ".card-title-custom" ? do
    color "#fff"
    position absolute
    bottom nil
    padding (px 20) (px 20) (px 20) (px 20)
    fontSize (px 24)
    fontWeight $ weight 300

  ".card-title-right" ? do
     right nil

  ".card-title-left" ? do
    right nil

  ".card-map" ? do
    display block
    borderRadius (px 2) (px 2) nil nil
    position relative
    width $ pct 100
    height $ px 300

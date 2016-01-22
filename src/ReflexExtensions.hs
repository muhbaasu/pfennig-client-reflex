{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module ReflexExtensions where

import           Data.Monoid ((<>))
import           Reflex
import           Reflex.Dom

metaViewport :: MonadWidget t m => String -> m ()
metaViewport s = elAttr "meta" ("name" =: "viewport" <> "content" =: s) blank

metaCharSet :: MonadWidget t m => String -> m ()
metaCharSet s = elAttr "meta" ("charset" =: s) blank

metaUtf8 ::  MonadWidget t m => m ()
metaUtf8 = metaCharSet "utf-8"

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
buttonClass :: MonadWidget t m  => String -> String -> m (Event t ())
buttonClass s c = do
  (e, _) <- elAttr' "button" ( "class" =: c
                            <> "type" =: "button") $ text s
  return $ domEvent Click e

formClass :: MonadWidget t m => String -> m ()
formClass c = elAttr "form" ("class" =: c) blank

iconClass :: MonadWidget t m => String -> String -> m ()
iconClass c t = elAttr "i" ("class" =: c) $ text t

row :: forall t m a. MonadWidget t m => m a -> m a
row = divClass "row"

rowClass :: forall t m a. MonadWidget t m => String ->m a -> m a
rowClass c= divClass $ "row " <> c


-- | Some utils for Halogen.
module Nonbili.Halogen
  ( when
  , unless
  , fromMaybe
  , attr
  , dataAttr
  , element
  , focus
  ) where

import Prelude hiding (when)

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Web.HTML.HTMLElement as HTMLElement

-- Helper to set `class=value`.
class_ :: forall r i. String -> HH.IProp ("class" :: String | r) i
class_ = HH.attr (HH.AttrName "class")

-- Helper to set `style=value`.
style :: forall r i. String -> HH.IProp ("style" :: String | r) i
style = HH.attr (HH.AttrName "style")

-- | Render when predicate is `true`.
when :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
when b f = if b then f unit else HH.text ""

-- | Render when predicate is `false`.
unless :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
unless b f = when (not b) f

-- | Render when predicate is `Just a`.
fromMaybe :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
fromMaybe m f = case m of
  Just v -> f v
  Nothing -> HH.text ""

-- | Helper to set attribute `name=value`.
attr :: forall r i. String -> String -> HH.IProp r i
attr name = HH.attr $ HH.AttrName name

-- | Helper to set attribute `data-name=value`.
dataAttr :: forall r i. String -> String -> HH.IProp r i
dataAttr name = attr $ "data-" <> name

-- | Helper to use arbitrary HTML tag or `<custom-element>`
element
  :: forall r w i
   . String
  -> Array (HH.IProp r i)
  -> Array (HH.HTML w i)
  -> HH.HTML w i
element name = HH.element (HH.ElemName name)

-- | Helper to focus an element with a `RefLabel`.
focus
  :: forall state act slot out m
   . MonadEffect m
  => H.RefLabel
  -> H.HalogenM state act slot out m Unit
focus ref = do
  H.getHTMLElementRef ref >>= traverse_ \el -> do
    liftEffect $ HTMLElement.focus el

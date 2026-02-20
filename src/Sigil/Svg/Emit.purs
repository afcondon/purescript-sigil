-- | Emit: LayoutNode → Effect Element.
-- |
-- | Uses shared attribute mapping from `Sigil.Svg.Attrs` and calls thin
-- | SVG DOM FFI helpers.
module Sigil.Svg.Emit
  ( emit
  , emitNode
  , renderClassDeclIntoSvg
  , renderSignatureIntoSvg
  , renderAdtIntoSvg
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Web.DOM (Element)

import Sigil.Types (RenderType, SuperclassInfo)
import Sigil.Svg.Types (LayoutNode, Dimensions)
import Sigil.Svg.Attrs (toSvgPrimitive)
import Sigil.Svg.Layout.ClassDef (layoutClassDef)
import Sigil.Svg.Layout.Signature (layoutSignature)
import Sigil.Svg.Layout.ADT (layoutADT)

foreign import createSvgElement :: String -> Effect Element
foreign import setAttr :: Element -> String -> String -> Effect Unit
foreign import appendChild :: Element -> Element -> Effect Unit
foreign import setTextContent :: Element -> String -> Effect Unit
foreign import _appendInto :: String -> Element -> Effect Unit

-- | Emit a layout tree as a detached SVG element with the given dimensions.
emit :: LayoutNode -> Dimensions -> Effect Element
emit layout dims = do
  svg <- createSvgElement "svg"
  let w = show dims.width
      h = show dims.height
  setAttr svg "width" w
  setAttr svg "height" h
  setAttr svg "viewBox" ("0 0 " <> w <> " " <> h)
  setAttr svg "style" "overflow: visible;"
  el <- emitNode layout
  appendChild svg el
  pure svg

-- | Emit a single LayoutNode to a DOM element.
emitNode :: LayoutNode -> Effect Element
emitNode node = do
  let prim = toSvgPrimitive node
  el <- createSvgElement prim.tag
  Array.foldM (\_ a -> setAttr el a.key a.value) unit prim.attrs
  case prim.textContent of
    Just t -> setTextContent el t
    Nothing -> pure unit
  Array.foldM (\_ child -> do
    childEl <- emitNode child
    appendChild el childEl
  ) unit prim.children
  pure el

-- | Render a class definition into a DOM element selected by CSS selector.
renderClassDeclIntoSvg
  :: String
  -> { name :: String
     , typeParams :: Array String
     , superclasses :: Array SuperclassInfo
     , methods :: Array { name :: String, ast :: Maybe RenderType }
     }
  -> Effect Unit
renderClassDeclIntoSvg selector opts = do
  let { layout, dimensions } = layoutClassDef opts
  svg <- emit layout dimensions
  _appendInto selector svg

-- | Render a signature into a DOM element selected by CSS selector.
renderSignatureIntoSvg
  :: String
  -> { name :: String
     , sig :: String
     , ast :: RenderType
     , typeParams :: Array String
     , className :: Maybe String
     }
  -> Effect Unit
renderSignatureIntoSvg selector opts = do
  let { layout, dimensions } = layoutSignature opts
  svg <- emit layout dimensions
  _appendInto selector svg

-- | Render an ADT into a DOM element selected by CSS selector.
renderAdtIntoSvg
  :: String
  -> { name :: String
     , typeParams :: Array String
     , constructors :: Array { name :: String, args :: Array RenderType }
     , keyword :: Maybe String
     }
  -> Effect Unit
renderAdtIntoSvg selector opts = do
  let { layout, dimensions } = layoutADT opts
  svg <- emit layout dimensions
  _appendInto selector svg

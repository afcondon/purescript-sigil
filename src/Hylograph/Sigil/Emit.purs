-- | Emit: LayoutNode → Effect Element.
-- |
-- | Pattern-matches on LayoutNode and calls thin SVG DOM FFI helpers.
module Hylograph.Sigil.Emit
  ( emit
  , emitNode
  ) where

import Prelude

import Data.Array as Array
import Effect (Effect)
import Web.DOM (Element)

import Hylograph.Sigil.Types.Layout (LayoutNode(..), Dimensions)

foreign import createSvgElement :: String -> Effect Element
foreign import setAttr :: Element -> String -> String -> Effect Unit
foreign import appendChild :: Element -> Element -> Effect Unit
foreign import setTextContent :: Element -> String -> Effect Unit

fontFamily :: String
fontFamily = "'Fira Code', 'SF Mono', 'Consolas', monospace"

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
emitNode = case _ of
  LText r -> do
    el <- createSvgElement "text"
    setAttr el "x" (show r.x)
    setAttr el "y" (show r.y)
    setAttr el "font-family" fontFamily
    setAttr el "font-size" (show r.fontSize)
    setAttr el "dominant-baseline" "middle"
    when (r.style /= "") (setAttr el "style" r.style)
    setTextContent el r.text
    pure el

  LRect r -> do
    el <- createSvgElement "rect"
    setAttr el "x" (show r.x)
    setAttr el "y" (show r.y)
    setAttr el "width" (show r.width)
    setAttr el "height" (show r.height)
    setAttr el "rx" (show r.rx)
    setAttr el "ry" (show r.rx)
    when (r.style /= "") (setAttr el "style" r.style)
    pure el

  LLine r -> do
    el <- createSvgElement "line"
    setAttr el "x1" (show r.x1)
    setAttr el "y1" (show r.y1)
    setAttr el "x2" (show r.x2)
    setAttr el "y2" (show r.y2)
    setAttr el "stroke" r.stroke
    setAttr el "stroke-width" (show r.strokeWidth)
    when (r.strokeLinecap /= "") (setAttr el "stroke-linecap" r.strokeLinecap)
    when (r.strokeDasharray /= "") (setAttr el "stroke-dasharray" r.strokeDasharray)
    pure el

  LCircle r -> do
    el <- createSvgElement "circle"
    setAttr el "cx" (show r.cx)
    setAttr el "cy" (show r.cy)
    setAttr el "r" (show r.r)
    setAttr el "fill" r.fill
    when (r.stroke /= "") (setAttr el "stroke" r.stroke)
    when (r.strokeWidth > 0.0) (setAttr el "stroke-width" (show r.strokeWidth))
    pure el

  LGroup r -> do
    el <- createSvgElement "g"
    when (r.transform /= "") (setAttr el "transform" r.transform)
    Array.foldM (\_ child -> do
      childEl <- emitNode child
      appendChild el childEl
    ) unit r.children
    pure el

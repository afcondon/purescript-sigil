-- | Shared LayoutNode → SVG attribute mapping.
-- |
-- | Both `Emit` (DOM FFI) and `Sigil.Hats` (HATS Tree) derive from
-- | `toSvgPrimitive` so attribute logic is defined once.
module Sigil.Svg.Attrs
  ( SvgPrimitive
  , toSvgPrimitive
  , fontFamily
  ) where

import Prelude

import Data.Maybe (Maybe(..))

import Sigil.Svg.Types (LayoutNode(..))

type SvgPrimitive =
  { tag :: String
  , attrs :: Array { key :: String, value :: String }
  , textContent :: Maybe String
  , children :: Array LayoutNode
  }

fontFamily :: String
fontFamily = "'Fira Code', 'SF Mono', 'Consolas', monospace"

toSvgPrimitive :: LayoutNode -> SvgPrimitive
toSvgPrimitive = case _ of
  LText r ->
    { tag: "text"
    , attrs:
        [ { key: "x", value: show r.x }
        , { key: "y", value: show r.y }
        , { key: "font-family", value: fontFamily }
        , { key: "font-size", value: show r.fontSize }
        , { key: "dominant-baseline", value: "middle" }
        ] <> if r.style == "" then [] else [ { key: "style", value: r.style } ]
    , textContent: Just r.text
    , children: []
    }

  LRect r ->
    { tag: "rect"
    , attrs:
        [ { key: "x", value: show r.x }
        , { key: "y", value: show r.y }
        , { key: "width", value: show r.width }
        , { key: "height", value: show r.height }
        , { key: "rx", value: show r.rx }
        , { key: "ry", value: show r.rx }
        ] <> if r.style == "" then [] else [ { key: "style", value: r.style } ]
    , textContent: Nothing
    , children: []
    }

  LLine r ->
    { tag: "line"
    , attrs:
        [ { key: "x1", value: show r.x1 }
        , { key: "y1", value: show r.y1 }
        , { key: "x2", value: show r.x2 }
        , { key: "y2", value: show r.y2 }
        , { key: "stroke", value: r.stroke }
        , { key: "stroke-width", value: show r.strokeWidth }
        ] <> if r.strokeLinecap == "" then [] else [ { key: "stroke-linecap", value: r.strokeLinecap } ]
          <> if r.strokeDasharray == "" then [] else [ { key: "stroke-dasharray", value: r.strokeDasharray } ]
    , textContent: Nothing
    , children: []
    }

  LCircle r ->
    { tag: "circle"
    , attrs:
        [ { key: "cx", value: show r.cx }
        , { key: "cy", value: show r.cy }
        , { key: "r", value: show r.r }
        , { key: "fill", value: r.fill }
        ] <> if r.stroke == "" then [] else [ { key: "stroke", value: r.stroke } ]
          <> if r.strokeWidth <= 0.0 then [] else [ { key: "stroke-width", value: show r.strokeWidth } ]
    , textContent: Nothing
    , children: []
    }

  LGroup r ->
    { tag: "g"
    , attrs:
        if r.transform == "" then [] else [ { key: "transform", value: r.transform } ]
    , textContent: Nothing
    , children: r.children
    }

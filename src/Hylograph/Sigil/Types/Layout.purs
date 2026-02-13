-- | Layout tree types for positioned SVG primitives.
module Hylograph.Sigil.Types.Layout
  ( LayoutNode(..)
  , Dimensions
  , RenderResult
  ) where

-- | A positioned SVG primitive. The entire layout pipeline produces these;
-- | only `Emit` touches the DOM.
data LayoutNode
  = LText
    { x :: Number
    , y :: Number
    , text :: String
    , fontSize :: Number
    , style :: String
    }
  | LRect
    { x :: Number
    , y :: Number
    , width :: Number
    , height :: Number
    , rx :: Number
    , style :: String
    }
  | LLine
    { x1 :: Number
    , y1 :: Number
    , x2 :: Number
    , y2 :: Number
    , stroke :: String
    , strokeWidth :: Number
    , strokeLinecap :: String
    , strokeDasharray :: String
    }
  | LCircle
    { cx :: Number
    , cy :: Number
    , r :: Number
    , fill :: String
    , stroke :: String
    , strokeWidth :: Number
    }
  | LGroup
    { transform :: String
    , children :: Array LayoutNode
    }

type Dimensions = { width :: Number, height :: Number }

-- | Internal result from node-level layout: positioned nodes + consumed width.
type RenderResult = { nodes :: Array LayoutNode, width :: Number }

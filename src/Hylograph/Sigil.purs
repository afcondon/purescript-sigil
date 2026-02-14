-- | Hylograph Sigil — Typographic SVG rendering for type signatures.
-- |
-- | Re-exports the public API.
module Hylograph.Sigil
  ( module Hylograph.Sigil.Types
  , module Hylograph.Sigil.Types.Layout
  , module Hylograph.Sigil.Layout.Signature
  , module Hylograph.Sigil.Layout.ADT
  , module Hylograph.Sigil.Layout.ClassDef
  , module Hylograph.Sigil.Layout.Sparkline
  , module Hylograph.Sigil.Layout.Siglet
  , module Hylograph.Sigil.Emit
  , module Hylograph.Sigil.Text
  , module Hylograph.Sigil.Color
  , module MeasureExports
  ) where

import Prim hiding (Constraint, Row)
import Hylograph.Sigil.Types (RenderType(..), RowField, Constraint, SuperclassInfo)
import Hylograph.Sigil.Types.Layout (LayoutNode(..), Dimensions, RenderResult)
import Hylograph.Sigil.Layout.Signature (layoutSignature)
import Hylograph.Sigil.Layout.ADT (layoutADT)
import Hylograph.Sigil.Layout.ClassDef (layoutClassDef)
import Hylograph.Sigil.Layout.Sparkline (layoutSparkline)
import Hylograph.Sigil.Layout.Siglet (layoutSiglet)
import Hylograph.Sigil.Emit (emit, emitNode)
import Hylograph.Sigil.Text (renderTypeToText, collectTypeVars, collectArrowParams, collectForallVars, fieldVars)
import Hylograph.Sigil.Color (assignVarColors)
import Hylograph.Sigil.Measure (RenderContext, defaultRenderContext, measure) as MeasureExports

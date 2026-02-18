-- | purescript-sigil — Typographic rendering for type signatures (HTML + SVG).
-- |
-- | Re-exports the public API.
module Sigil
  ( module Sigil.Types
  , module Sigil.Text
  , module Sigil.Color
  , module Sigil.Parse
  , module Sigil.Html
  , module Sigil.Svg.Types
  , module Sigil.Svg.Layout.Signature
  , module Sigil.Svg.Layout.ADT
  , module Sigil.Svg.Layout.ClassDef
  , module Sigil.Svg.Layout.Sparkline
  , module Sigil.Svg.Layout.Siglet
  , module Sigil.Svg.Emit
  , module MeasureExports
  ) where

import Prim hiding (Constraint, Row)
import Sigil.Types (RenderType(..), RowField, Constraint, SuperclassInfo)
import Sigil.Text (renderTypeToText, collectTypeVars, collectArrowParams, collectForallVars, fieldVars)
import Sigil.Color (assignVarColors, isEffectName, isEffectNameIn, defaultEffectNames)
import Sigil.Parse (parseToRenderType, extractCtorArgs, extractCtorRenderTypes, elideAST, elideConstraint)
import Sigil.Html (renderSignature, renderBody, renderSiglet, renderSignet, renderDataDecl, renderClassDecl, renderTypeSynonym, renderForeignImport, renderSignatureInto, renderBodyInto, renderSigletInto, renderSignetInto, renderDataDeclInto, renderClassDeclInto, renderTypeSynonymInto, renderForeignImportInto, peelSignature, escapeHtml)
import Sigil.Svg.Types (LayoutNode(..), Dimensions, RenderResult)
import Sigil.Svg.Layout.Signature (layoutSignature)
import Sigil.Svg.Layout.ADT (layoutADT)
import Sigil.Svg.Layout.ClassDef (layoutClassDef)
import Sigil.Svg.Layout.Sparkline (layoutSparkline)
import Sigil.Svg.Layout.Siglet (layoutSiglet)
import Sigil.Svg.Emit (emit, emitNode)
import Sigil.Svg.Measure (RenderContext, defaultRenderContext, measure) as MeasureExports

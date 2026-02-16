-- | Siglet layout: elided type signature showing arity, structure, and variable patterns.
-- |
-- | Concrete type names become colored circles; type variable pills keep their colors.
-- | Forall and constraint annotations are rendered in the same zones as the
-- | full-size sigil: constraints above, body middle, forall below.
module Sigil.Svg.Layout.Siglet
  ( layoutSiglet
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String.CodeUnits as SCU

import Sigil.Types (RenderType)
import Sigil.Svg.Types (LayoutNode(..))
import Sigil.Color (colors, assignVarColors)
import Sigil.Text (collectForallVars, collectTypeVars)
import Sigil.Svg.Measure (RenderContext, defaultRenderContext, measure)
import Sigil.Svg.Layout (unwrapType, renderNode, renderSmallPill, renderSigletVarDot, renderConstraintPile)

-- | Lay out a siglet (elided miniature type signature).
-- | TCon names render as small circles; TVar pills keep colors.
-- | Layout mirrors the full-size sigil: constraint pile above, body middle,
-- | forall row below.
-- | Returns Nothing if the type can't fit.
layoutSiglet
  :: { ast :: RenderType
     , maxWidth :: Number
     , maxHeight :: Number
     }
  -> Maybe { layout :: LayoutNode, scaledWidth :: Number, scaledHeight :: Number }
layoutSiglet opts =
  let
    allVars = collectForallVars opts.ast
               <> (Set.toUnfoldable (collectTypeVars opts.ast) :: Array String)
    varColors = assignVarColors allVars
    ctx = defaultRenderContext { varColors = varColors, sigletMode = true }

    -- Unwrap forall/constraints, matching full-size sigil structure
    { forallVars, constraints, body } = unwrapType opts.ast

    bodyM = measure ctx body

    constraintH = if Array.length constraints > 0
      then toNumber (Array.length constraints) * 24.0 + 10.0
      else 0.0
    forallH = if Array.length forallVars > 0 then 22.0 else 0.0
    bodyH = max bodyM.height ctx.lineHeight

    fullW = bodyM.width + 10.0
    fullH = constraintH + bodyH + forallH + 4.0
  in
    if fullW <= 0.0 || fullH <= 0.0 then Nothing
    else
      let
        scaleX = opts.maxWidth / fullW
        scaleY = opts.maxHeight / fullH
        scale = min (min scaleX scaleY) 1.0

        -- Constraint pile above body
        constraintNodes = if Array.length constraints > 0
          then (renderConstraintPile ctx 0.0 0.0 constraints).nodes
          else []

        -- Body (arrow chain, applied types, etc.)
        bodyR = renderNode ctx 0.0 constraintH body

        -- Forall row below body
        forallNodes = if Array.length forallVars > 0
          then buildForallRow ctx forallVars (constraintH + bodyH + 4.0)
          else []

        inner = LGroup
          { transform: "scale(" <> show scale <> ")"
          , children: constraintNodes <> bodyR.nodes <> forallNodes
          }

        layout = LGroup
          { transform: ""
          , children: [inner]
          }
      in Just
        { layout
        , scaledWidth: fullW * scale
        , scaledHeight: fullH * scale
        }

-- | Forall row: ∀ symbol followed by variable pills/dots.
-- | In siglet mode, multi-letter vars become colored dots.
buildForallRow :: RenderContext -> Array String -> Number -> Array LayoutNode
buildForallRow ctx forallVars curY =
  let
    forallSymbol = LText
      { x: 0.0, y: curY + 9.0, text: "\x2200"
      , fontSize: 14.0, style: "fill:" <> colors.keyword <> ";font-weight:700;" }
    fx0 = ctx.charWidth * 1.4 + 3.0
    pillsAcc = Array.foldl (\acc v ->
      let
        cx = if acc.first then acc.fx else acc.fx + 3.0
        pillR = if SCU.length v > 1
          -- Multi-letter var → colored dot; offset y so circle centers at curY + 9
          then renderSigletVarDot ctx cx (curY - 1.0) v
          else renderSmallPill ctx cx (curY + 1.0) v (curY + 9.0)
      in { fx: cx + pillR.width, nodes: acc.nodes <> pillR.nodes, first: false }
    ) { fx: fx0, nodes: [], first: true } forallVars
  in [forallSymbol] <> pillsAcc.nodes

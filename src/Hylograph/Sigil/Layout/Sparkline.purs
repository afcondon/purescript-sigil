-- | Sparkline layout: miniature type signature for treemap cells.
module Hylograph.Sigil.Layout.Sparkline
  ( layoutSparkline
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Set as Set

import Hylograph.Sigil.Types (RenderType)
import Hylograph.Sigil.Types.Layout (LayoutNode(..))
import Hylograph.Sigil.Color (colors, assignVarColors)
import Hylograph.Sigil.Text (collectForallVars, collectTypeVars)
import Hylograph.Sigil.Measure (defaultRenderContext, measure)
import Hylograph.Sigil.Layout (unwrapType, renderNode, renderSmallPill, renderConstraintPile)

-- | Lay out a sparkline miniature.
layoutSparkline
  :: { ast :: RenderType
     , maxWidth :: Number
     , maxHeight :: Number
     }
  -> Maybe { layout :: LayoutNode, scaledWidth :: Number, scaledHeight :: Number }
layoutSparkline opts =
  let
    { forallVars, constraints, body } = unwrapType opts.ast
    allVars = collectForallVars opts.ast
               <> (Set.toUnfoldable (collectTypeVars opts.ast) :: Array String)
    varColors = assignVarColors (forallVars <> allVars)
    ctx = defaultRenderContext { varColors = varColors }

    bodyM = measure ctx body
    constraintH = if Array.length constraints > 0 then
      min (toNumber (Array.length constraints)) 3.0 * 24.0 + 10.0 else 0.0
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

        -- Build inner content
        constraintNodes = if Array.length constraints > 0 then
          renderConstraintPile ctx 0.0 0.0 constraints
        else { nodes: [], width: 0.0 }

        bodyR = renderNode ctx 0.0 constraintH body

        forallNodes = if Array.length forallVars > 0 then
          let
            fy = constraintH + bodyH + 2.0
            forallSymbol = LText
              { x: 0.0, y: fy + 9.0, text: "\x2200"
              , fontSize: 14.0, style: "fill:" <> colors.keyword <> ";font-weight:700;" }
            fx0 = ctx.charWidth * 1.4 + 3.0
            pillsAcc = Array.foldl (\acc v ->
              let
                fx = if acc.first then acc.fx else acc.fx + 3.0
                pillR = renderSmallPill ctx fx (fy + 1.0) v (fy + 9.0)
              in { fx: fx + pillR.width, nodes: acc.nodes <> pillR.nodes, first: false }
            ) { fx: fx0, nodes: [], first: true } forallVars
          in [forallSymbol] <> pillsAcc.nodes
        else []

        inner = LGroup
          { transform: "scale(" <> show scale <> ")"
          , children: constraintNodes.nodes <> bodyR.nodes <> forallNodes
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

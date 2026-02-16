-- | Signature layout: value declarations, type synonyms.
module Sigil.Svg.Layout.Signature
  ( layoutSignature
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Set as Set

import Sigil.Types (RenderType)
import Sigil.Svg.Types (LayoutNode(..), Dimensions)
import Sigil.Color (colors, assignVarColors)
import Sigil.Text (collectForallVars, collectTypeVars)
import Sigil.Svg.Measure (defaultRenderContext, textWidth, measure)
import Sigil.Svg.Layout (unwrapType, parseClassName, renderNode, renderSmallPill, renderConstraintPile)

-- | Lay out a value/type-synonym signature.
layoutSignature
  :: { name :: String
     , sig :: String
     , ast :: RenderType
     , typeParams :: Array String
     , className :: Maybe String
     }
  -> { layout :: LayoutNode, dimensions :: Dimensions }
layoutSignature opts =
  let
    classInfo = opts.className >>= parseClassName
    typeParams = opts.typeParams

    { forallVars, constraints, body } = unwrapType opts.ast

    -- Collect all vars for color assignment
    classParams = case classInfo of
      Just ci -> ci.params
      Nothing -> []
    allVars = collectForallVars opts.ast
    allTypeVars = Array.nub (typeParams <> classParams <> forallVars <> allVars
                             <> (Set.toUnfoldable (collectTypeVars opts.ast) :: Array String))
    varColors = assignVarColors (typeParams <> classParams <> forallVars <> allTypeVars)
    ctx = defaultRenderContext { varColors = varColors }

    -- Layout dimensions
    pillW = if Array.length typeParams > 0 then
      Array.foldl (\w p -> w + textWidth ctx.charWidth p + 14.0 + 3.0) 4.0 typeParams
    else 0.0
    nameW = textWidth ctx.charWidth opts.name + pillW + textWidth ctx.charWidth " :: "
    bodyM = measure ctx body

    classH = case classInfo of
      Just _ -> 20.0
      Nothing -> 0.0
    forallH = if Array.length forallVars > 0 then 22.0 else 0.0
    constraintH = if Array.length constraints > 0 then
      toNumber (Array.length constraints) * 24.0 + 10.0 else 0.0
    bodyH = max bodyM.height ctx.lineHeight
    totalW = nameW + bodyM.width + 40.0
    totalH = classH + constraintH + bodyH + forallH + 16.0

    svgW = max totalW 100.0
    svgH = max totalH 36.0

    -- Build layout nodes
    nodes = buildNodes ctx opts.name typeParams classInfo forallVars constraints body
              nameW bodyM classH forallH constraintH bodyH

    layout = LGroup
      { transform: "translate(10, 8)"
      , children: nodes
      }
  in
    { layout
    , dimensions: { width: svgW, height: svgH }
    }
  where
  buildNodes ctx' name typeParams classInfo forallVars constraints body
             nameW _bodyM classH _forallH constraintH bodyH =
    let
      -- Class membership annotation
      classNodes = case classInfo of
        Just ci -> buildClassRow ctx' ci 0.0
        Nothing -> { nodes: [], curY: 0.0 }

      curY0 = classNodes.curY + case classInfo of
        Just _ -> classH
        Nothing -> 0.0

      -- Constraint pile
      constraintNodes = if Array.length constraints > 0 then
        renderConstraintPile ctx' nameW curY0 constraints
      else { nodes: [], width: 0.0 }

      curY1 = curY0 + constraintH

      -- Declaration name
      nameNode = LText
        { x: 0.0, y: curY1 + ctx'.lineHeight / 2.0, text: name
        , fontSize: ctx'.fontSize, style: "fill:" <> colors.name <> ";font-weight:700;" }

      -- Type parameter pills
      paramNodes = if Array.length typeParams > 0 then
        buildTypeParamPills ctx' typeParams (textWidth ctx'.charWidth name) curY1
      else { nodes: [], endX: textWidth ctx'.charWidth name }

      -- ::
      sepNode = LText
        { x: paramNodes.endX, y: curY1 + ctx'.lineHeight / 2.0, text: " :: "
        , fontSize: ctx'.fontSize, style: "fill:" <> colors.separator <> ";" }

      -- Body
      bodyR = renderNode ctx' nameW curY1 body

      curY2 = curY1 + bodyH

      -- Forall annotation
      forallNodes = if Array.length forallVars > 0 then
        buildForallRow ctx' forallVars (curY2 + 4.0)
      else []
    in
      classNodes.nodes <> constraintNodes.nodes
        <> [nameNode] <> paramNodes.nodes <> [sepNode]
        <> bodyR.nodes <> forallNodes

  buildClassRow ctx' ci curY =
    let
      kwNode = LText
        { x: 0.0, y: curY + 10.0, text: "class"
        , fontSize: 10.0
        , style: "fill:" <> colors.classBd <> ";font-weight:600;letter-spacing:0.5px;" }
      kwW = textWidth ctx'.charWidth "class" + 6.0
      nameNode = LText
        { x: kwW, y: curY + 10.0, text: ci.name
        , fontSize: 11.0, style: "fill:" <> colors.classText <> ";font-weight:700;" }
      nameEndX = kwW + textWidth ctx'.charWidth ci.name + 4.0
      paramAcc = Array.foldl (\acc p ->
        let pillR = renderSmallPill ctx' acc.cx curY p (curY + 10.0)
        in { cx: acc.cx + pillR.width + 3.0, nodes: acc.nodes <> pillR.nodes }
      ) { cx: nameEndX, nodes: [] } ci.params
    in
      { nodes: [kwNode, nameNode] <> paramAcc.nodes, curY }

  buildTypeParamPills ctx' typeParams startX curY =
    let
      acc = Array.foldl (\a p ->
        let pillR = renderSmallPill ctx' a.endX curY p (curY + ctx'.lineHeight / 2.0)
        in { endX: a.endX + pillR.width + 3.0, nodes: a.nodes <> pillR.nodes }
      ) { endX: startX + 4.0, nodes: [] } typeParams
    in acc

  buildForallRow ctx' forallVars curY =
    let
      forallSymbol = LText
        { x: 0.0, y: curY + 9.0, text: "\x2200"
        , fontSize: 14.0, style: "fill:" <> colors.keyword <> ";font-weight:700;" }
      fx0 = ctx'.charWidth * 1.4 + 3.0
      pillsAcc = Array.foldl (\acc v ->
        let
          cx = if acc.first then acc.fx else acc.fx + 3.0
          pillR = renderSmallPill ctx' cx (curY + 1.0) v (curY + 9.0)
        in { fx: cx + pillR.width, nodes: acc.nodes <> pillR.nodes, first: false }
      ) { fx: fx0, nodes: [], first: true } forallVars
    in [forallSymbol] <> pillsAcc.nodes

-- | Class definition layout: header, superclasses, own methods, inherited methods.
module Sigil.Svg.Layout.ClassDef
  ( layoutClassDef
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set as Set

import Sigil.Types (RenderType, SuperclassInfo)
import Sigil.Svg.Types (LayoutNode(..), Dimensions)
import Sigil.Color (colors, assignVarColors)
import Sigil.Text (constraintText, collectForallVars, collectTypeVars)
import Sigil.Svg.Measure (RenderContext, defaultRenderContext, textWidth, measure)
import Sigil.Svg.Layout (unwrapType, renderNode, renderSmallPill)

-- | Lay out a type class definition.
layoutClassDef
  :: { name :: String
     , typeParams :: Array String
     , superclasses :: Array SuperclassInfo
     , methods :: Array { name :: String, ast :: Maybe RenderType }
     }
  -> { layout :: LayoutNode, dimensions :: Dimensions }
layoutClassDef opts =
  let
    baseVarColors = assignVarColors opts.typeParams
    ctx0 = defaultRenderContext { varColors = baseVarColors }

    headerH = 28.0
    hasSuperclasses = not (Array.null opts.superclasses)
    superH = if hasSuperclasses then 22.0 else 0.0

    -- Own method stats
    methodStats = map (\m -> calcMethodStats ctx0 opts.typeParams m) opts.methods
    totalMethodH = Array.foldl (\acc s -> acc + s.height) 0.0 methodStats
    totalMethodH' = if Array.null opts.methods then 30.0 else totalMethodH
    maxMethodW = Array.foldl (\acc s -> max acc s.width)
      (textWidth ctx0.charWidth ("class " <> opts.name) + 80.0) methodStats

    -- Inherited method stats (from superclasses)
    hasInheritedMethods = hasSuperclasses &&
      Array.any (\sc -> not (Array.null sc.methods)) opts.superclasses
    inheritedStats = if hasInheritedMethods
      then computeInheritedStats ctx0 opts.typeParams opts.superclasses
      else { height: 0.0, width: 0.0 }
    -- Add separator + section padding if inherited methods present
    inheritedSectionH = if hasInheritedMethods
      then 12.0 + inheritedStats.height  -- 12px for dashed separator area
      else 0.0

    totalH = headerH + superH + totalMethodH' + inheritedSectionH + 16.0
    totalW = max (max maxMethodW inheritedStats.width) 200.0

    -- Header bar
    headerRect = LRect
      { x: 0.0, y: 0.0, width: totalW, height: headerH, rx: 4.0
      , style: "fill:" <> colors.classBg <> ";stroke:" <> colors.classBd <> ";stroke-width:1;" }
    classKw = LText
      { x: 8.0, y: headerH / 2.0, text: "class "
      , fontSize: 12.0, style: "fill:" <> colors.keyword <> ";font-weight:600;" }
    classKwW = textWidth ctx0.charWidth "class "
    nameNode = LText
      { x: 8.0 + classKwW, y: headerH / 2.0, text: opts.name
      , fontSize: ctx0.fontSize, style: "fill:" <> colors.classText <> ";font-weight:700;" }
    paramStartX = 8.0 + classKwW + textWidth ctx0.charWidth opts.name + 6.0
    paramAcc = Array.foldl (\acc p ->
      let pillR = renderSmallPill ctx0 acc.px 6.0 p (headerH / 2.0)
      in { px: acc.px + pillR.width + 3.0, nodes: acc.nodes <> pillR.nodes }
    ) { px: paramStartX, nodes: [] } opts.typeParams

    -- Superclass names row
    superNames = map _.name opts.superclasses
    superNodes = buildSuperclassRow superNames headerH

    -- Own methods
    methodStartY = headerH + superH
    methodsResult = buildMethods ctx0 opts.typeParams opts.methods methodStats totalW methodStartY

    -- Inherited methods section
    inheritedStartY = methodStartY + totalMethodH'
    inheritedNodes = if hasInheritedMethods
      then buildInheritedSection ctx0 opts.typeParams opts.superclasses totalW inheritedStartY
      else []

    layout = LGroup
      { transform: ""
      , children: [headerRect, classKw, nameNode] <> paramAcc.nodes
                   <> superNodes <> methodsResult <> inheritedNodes
      }
  in
    { layout, dimensions: { width: totalW, height: totalH } }

type MethodStats = { height :: Number, width :: Number }

calcMethodStats :: RenderContext -> Array String -> { name :: String, ast :: Maybe RenderType } -> MethodStats
calcMethodStats ctx _typeParams m = case m.ast of
  Just ast ->
    let
      { forallVars, constraints, body } = unwrapType ast
      bodyM = measure ctx body
      bodyH = max ctx.lineHeight bodyM.height
      mNameW = textWidth ctx.charWidth (m.name <> " :: ")
      mW = mNameW + bodyM.width + 40.0
      h0 = 8.0 -- top padding
      h1 = if Array.length constraints > 0 then h0 + 20.0 else h0
      h2 = h1 + bodyH
      h3 = if Array.length forallVars > 0 then h2 + 21.0 else h2
      h4 = h3 + 8.0 -- bottom padding
    in { height: h4, width: mW }
  Nothing ->
    { height: 32.0, width: textWidth ctx.charWidth m.name + 80.0 }

buildSuperclassRow :: Array String -> Number -> Array LayoutNode
buildSuperclassRow superclasses headerH =
  if Array.null superclasses then []
  else
    let
      ctx = defaultRenderContext
      -- Scale charWidth for the 10px font used in superclass row
      scCharW = ctx.charWidth * 10.0 / ctx.fontSize
      arrowNode = LText
        { x: 12.0, y: headerH + 11.0, text: "\x2190"
        , fontSize: 11.0, style: "fill:" <> colors.constraintBd <> ";" }
      startX = 12.0 + scCharW * 1.5 + 4.0
      scAcc = Array.foldl (\acc idx ->
        case Array.index superclasses idx of
          Nothing -> acc
          Just sc ->
            let
              comma = if idx > 0 then
                [ LText { x: acc.sx, y: headerH + 11.0, text: ", "
                        , fontSize: 10.0, style: "fill:" <> colors.separator <> ";" } ]
              else []
              commaW = if idx > 0 then scCharW * 2.0 else 0.0
              scNode = LText
                { x: acc.sx + commaW, y: headerH + 11.0, text: sc
                , fontSize: 10.0, style: "fill:" <> colors.constraint <> ";font-weight:600;" }
            in { sx: acc.sx + commaW + textWidth scCharW sc + 3.0
               , nodes: acc.nodes <> comma <> [scNode]
               }
      ) { sx: startX, nodes: [] } (Array.range 0 (Array.length superclasses - 1))
    in [arrowNode] <> scAcc.nodes

buildMethods :: RenderContext -> Array String -> Array { name :: String, ast :: Maybe RenderType }
             -> Array MethodStats -> Number -> Number -> Array LayoutNode
buildMethods ctx0 typeParams methods stats totalW startY =
  if Array.null methods then
    [ LText { x: 16.0, y: startY + 20.0, text: "(no own methods)"
            , fontSize: 11.0, style: "fill:#aaa;font-style:italic;" } ]
  else
    let
      result = Array.foldl (\acc idx ->
        case Array.index methods idx of
          Nothing -> acc
          Just m ->
            let
              curY = acc.curY + 8.0
              separator = if idx > 0 then
                [ LLine { x1: 8.0, y1: curY - 2.0, x2: totalW - 8.0, y2: curY - 2.0
                         , stroke: "#e8e8f0", strokeWidth: 1.0
                         , strokeLinecap: "", strokeDasharray: "" } ]
              else []
              methodNodes = renderMethod ctx0 typeParams m curY
              statHeight = case Array.index stats idx of
                Just s -> s.height
                Nothing -> 32.0
            in { curY: acc.curY + statHeight, nodes: acc.nodes <> separator <> methodNodes.nodes }
      ) { curY: startY, nodes: [] } (Array.range 0 (Array.length methods - 1))
    in result.nodes

-- | Compute total height and max width for the inherited methods section.
computeInheritedStats :: RenderContext -> Array String -> Array SuperclassInfo
                      -> { height :: Number, width :: Number }
computeInheritedStats ctx0 typeParams superclasses =
  let
    result = Array.foldl (\acc sc ->
      if Array.null sc.methods then acc
      else
        let
          -- "via ClassName" header: 20px
          headerH = 20.0
          headerW = textWidth ctx0.charWidth ("via " <> sc.name) + 40.0
          mStats = map (\m -> calcMethodStats ctx0 typeParams m) sc.methods
          mTotalH = Array.foldl (\a s -> a + s.height) 0.0 mStats
          mMaxW = Array.foldl (\a s -> max a s.width) headerW mStats
        in { height: acc.height + headerH + mTotalH
           , width: max acc.width mMaxW
           }
    ) { height: 0.0, width: 0.0 } superclasses
  in result

-- | Build the inherited methods section: dashed separator + "via ClassName:" groups.
buildInheritedSection :: RenderContext -> Array String -> Array SuperclassInfo
                      -> Number -> Number -> Array LayoutNode
buildInheritedSection ctx0 typeParams superclasses totalW startY =
  let
    -- Dashed separator line
    sepY = startY + 6.0
    separator =
      [ LLine { x1: 8.0, y1: sepY, x2: totalW - 8.0, y2: sepY
               , stroke: "#d4d4d8", strokeWidth: 1.0
               , strokeLinecap: "", strokeDasharray: "4 3" } ]

    -- Render each superclass group
    result = Array.foldl (\acc sc ->
      if Array.null sc.methods then acc
      else
        let
          curY = acc.curY
          -- "via ClassName" header
          viaLabel = LText
            { x: 12.0, y: curY + 12.0, text: "via " <> sc.name
            , fontSize: 10.0, style: "fill:" <> colors.constraint <> ";font-weight:600;font-style:italic;" }

          -- Render methods for this superclass (using semi-transparent group)
          methodAcc = Array.foldl (\mAcc m ->
            let
              mNodes = renderMethod ctx0 typeParams m mAcc.my
              stats = calcMethodStats ctx0 typeParams m
            in { my: mAcc.my + stats.height, nodes: mAcc.nodes <> mNodes.nodes }
          ) { my: curY + 20.0, nodes: [] } sc.methods

          groupNodes = LGroup
            { transform: ""
            , children: [viaLabel] <> methodAcc.nodes
            }
        in { curY: methodAcc.my, nodes: acc.nodes <> [groupNodes] }
    ) { curY: startY + 12.0, nodes: [] } superclasses

  in separator <> result.nodes

renderMethod :: RenderContext -> Array String -> { name :: String, ast :: Maybe RenderType } -> Number
             -> { nodes :: Array LayoutNode, curY :: Number }
renderMethod ctx0 typeParams m curY = case m.ast of
  Just ast ->
    let
      { forallVars, constraints, body } = unwrapType ast
      methodVars = typeParams <> collectForallVars ast
                   <> (Set.toUnfoldable (collectTypeVars ast) :: Array String)
      varColors = assignVarColors methodVars
      ctx = ctx0 { varColors = varColors }
      mNameW = textWidth ctx.charWidth (m.name <> " :: ")

      -- Constraints (inline, above method)
      constraintAcc = if Array.length constraints > 0 then
        let
          cNodes = Array.foldl (\acc c ->
            let
              ct = constraintText c
              cw = textWidth ctx.charWidth ct + 12.0
              cRect = LRect
                { x: acc.cx, y: curY + 2.0, width: cw, height: 16.0, rx: 3.0
                , style: "fill:" <> colors.constraintBg <> ";stroke:" <> colors.constraintBd <> ";stroke-width:0.75;" }
              cText = LText
                { x: acc.cx + 6.0, y: curY + 10.0, text: ct
                , fontSize: 10.0, style: "fill:" <> colors.constraint <> ";font-weight:600;" }
            in { cx: acc.cx + cw + 4.0, nodes: acc.nodes <> [cRect, cText] }
          ) { cx: 12.0 + mNameW, nodes: [] } constraints
        in { nodes: cNodes.nodes, dy: 20.0 }
      else { nodes: [], dy: 0.0 }

      curY' = curY + constraintAcc.dy

      -- Method name + body
      bodyM = measure ctx body
      bodyH = max ctx.lineHeight bodyM.height
      nameNode = LText
        { x: 12.0, y: curY' + ctx.lineHeight / 2.0, text: m.name
        , fontSize: 12.0, style: "fill:" <> colors.name <> ";font-weight:700;" }
      sepNode = LText
        { x: 12.0 + textWidth ctx.charWidth m.name, y: curY' + ctx.lineHeight / 2.0, text: " :: "
        , fontSize: 12.0, style: "fill:" <> colors.separator <> ";" }
      bodyR = renderNode ctx (12.0 + mNameW) curY' body

      curY'' = curY' + bodyH

      -- Forall underneath method name
      forallNodes = if Array.length forallVars > 0 then
        let
          fy = curY'' + 3.0
          forallSymbol = LText
            { x: 12.0, y: fy + 8.0, text: "\x2200"
            , fontSize: 13.0, style: "fill:" <> colors.keyword <> ";font-weight:700;" }
          fx0 = 12.0 + ctx.charWidth * 1.2
          pillsAcc = Array.foldl (\acc v ->
            let
              fx = if acc.first then acc.fx else acc.fx + 2.0
              pillR = renderSmallPill ctx fx fy v (fy + 8.0)
            in { fx: fx + pillR.width, nodes: acc.nodes <> pillR.nodes, first: false }
          ) { fx: fx0, nodes: [], first: true } forallVars
        in { nodes: [forallSymbol] <> pillsAcc.nodes, dy: 18.0 }
      else { nodes: [], dy: 0.0 }

      finalCurY = curY'' + forallNodes.dy + 8.0
    in
      { nodes: constraintAcc.nodes <> [nameNode, sepNode] <> bodyR.nodes <> forallNodes.nodes
      , curY: finalCurY
      }

  Nothing ->
    let
      fallback = LText
        { x: 12.0, y: curY + 10.0, text: m.name
        , fontSize: 11.0, style: "fill:#999;" }
    in { nodes: [fallback], curY: curY + 24.0 }

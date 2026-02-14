-- | ADT layout: data types with constructor branches.
module Hylograph.Sigil.Layout.ADT
  ( layoutADT
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))

import Hylograph.Sigil.Types (RenderType)
import Hylograph.Sigil.Types.Layout (LayoutNode(..), Dimensions)
import Hylograph.Sigil.Color (colors, adtColors, assignVarColors)
import Hylograph.Sigil.Measure (defaultRenderContext, textWidth, measure)
import Hylograph.Sigil.Layout (renderNode, renderSmallPill)

adtRowHeight :: Number
adtRowHeight = 28.0

adtRailX :: Number
adtRailX = 16.0

adtCtorX :: Number
adtCtorX = 40.0

-- | Lay out a data type with constructor branches.
layoutADT
  :: { name :: String
     , typeParams :: Array String
     , constructors :: Array { name :: String, args :: Array RenderType }
     , keyword :: Maybe String
     }
  -> { layout :: LayoutNode, dimensions :: Dimensions }
layoutADT opts =
  let
    varColors = assignVarColors opts.typeParams
    ctx = defaultRenderContext { varColors = varColors }

    kw = case opts.keyword of
      Just k -> k <> " "
      Nothing -> "data "

    -- Header text width
    headerText = kw <> opts.name <> if Array.length opts.typeParams > 0
      then " " <> Array.intercalate " " opts.typeParams else ""
    headerW = textWidth ctx.charWidth headerText + 24.0

    ctorCount = Array.length opts.constructors
    isOpaque = ctorCount == 0
  in
    if isOpaque
      then layoutOpaque opts.name kw headerW ctx opts.typeParams
      else layoutWithConstructors opts.name kw headerW ctx opts.typeParams opts.constructors ctorCount

-- | Layout for opaque types (no public constructors).
-- | Shows header + a subtle "no public constructors" indicator.
layoutOpaque
  :: String -> String -> Number
  -> { charWidth :: Number, fontSize :: Number, lineHeight :: Number | _ }
  -> Array String
  -> { layout :: LayoutNode, dimensions :: Dimensions }
layoutOpaque name kw headerW ctx typeParams =
  let
    varColors = assignVarColors typeParams
    ctxV = defaultRenderContext { varColors = varColors }

    opaqueText = "no public constructors"
    opaqueW = textWidth ctx.charWidth opaqueText + 24.0
    totalW = max (max headerW opaqueW) 180.0
    headerH = 28.0
    opaqueAreaTop = headerH + 10.0
    opaqueH = 24.0
    totalH = opaqueAreaTop + opaqueH + 8.0

    -- Header bar
    headerRect = LRect
      { x: 0.0, y: 0.0, width: totalW, height: headerH, rx: 4.0
      , style: "fill:" <> adtColors.headerBg <> ";stroke:" <> adtColors.headerBd <> ";stroke-width:1;" }
    dataKw = LText
      { x: 8.0, y: headerH / 2.0, text: kw
      , fontSize: 12.0, style: "fill:" <> colors.keyword <> ";font-weight:600;" }
    dataKwW = textWidth ctx.charWidth kw
    nameNode = LText
      { x: 8.0 + dataKwW, y: headerH / 2.0, text: name
      , fontSize: ctx.fontSize, style: "fill:" <> adtColors.headerText <> ";font-weight:700;" }
    paramStartX = 8.0 + dataKwW + textWidth ctx.charWidth name + 6.0
    paramAcc = Array.foldl (\acc p ->
      let pillR = renderSmallPill ctxV acc.px 6.0 p (headerH / 2.0)
      in { px: acc.px + pillR.width + 3.0, nodes: acc.nodes <> pillR.nodes }
    ) { px: paramStartX, nodes: [] } typeParams

    -- Opaque indicator: dashed pill with italic text
    opaqueCy = opaqueAreaTop + opaqueH / 2.0
    opaquePillW = textWidth ctx.charWidth opaqueText + 16.0
    opaquePillX = (totalW - opaquePillW) / 2.0  -- centered
    opaquePill = LRect
      { x: opaquePillX, y: opaqueAreaTop, width: opaquePillW, height: opaqueH, rx: 12.0
      , style: "fill:none;stroke:#ccc;stroke-width:1;stroke-dasharray:3 2;" }
    opaqueLabel = LText
      { x: totalW / 2.0, y: opaqueCy
      , text: opaqueText
      , fontSize: 10.0, style: "fill:#bbb;font-style:italic;text-anchor:middle;" }

    layout = LGroup
      { transform: ""
      , children: [headerRect, dataKw, nameNode] <> paramAcc.nodes <> [opaquePill, opaqueLabel]
      }
  in
    { layout, dimensions: { width: totalW, height: totalH } }

-- | Layout for ADTs with visible constructors.
layoutWithConstructors
  :: String -> String -> Number
  -> { charWidth :: Number, fontSize :: Number, lineHeight :: Number | _ }
  -> Array String
  -> Array { name :: String, args :: Array RenderType }
  -> Int
  -> { layout :: LayoutNode, dimensions :: Dimensions }
layoutWithConstructors name kw headerW ctx typeParams constructors ctorCount =
  let
    varColors = assignVarColors typeParams
    ctxV = defaultRenderContext { varColors = varColors }

    -- Max constructor name width
    maxCtorNameW = Array.foldl (\acc ctor ->
      max acc (textWidth ctx.charWidth ctor.name)
    ) 0.0 constructors

    -- Max args width
    maxArgsW = Array.foldl (\acc ctor ->
      let argsW = Array.foldl (\{ w, first } arg ->
            let argW = (measure ctxV arg).width
                spacing = if first then 0.0 else ctx.charWidth
            in { w: w + spacing + argW, first: false }
          ) { w: 0.0, first: true } ctor.args
      in max acc argsW.w
    ) 0.0 constructors

    argX = adtCtorX + maxCtorNameW + ctx.charWidth * 2.0
    totalW = max (max headerW (argX + maxArgsW + 20.0)) 180.0
    headerH = 28.0
    ctorAreaTop = headerH + 10.0
    totalH = ctorAreaTop + toNumber ctorCount * adtRowHeight + 8.0

    -- Header bar
    headerRect = LRect
      { x: 0.0, y: 0.0, width: totalW, height: headerH, rx: 4.0
      , style: "fill:" <> adtColors.headerBg <> ";stroke:" <> adtColors.headerBd <> ";stroke-width:1;" }
    dataKw = LText
      { x: 8.0, y: headerH / 2.0, text: kw
      , fontSize: 12.0, style: "fill:" <> colors.keyword <> ";font-weight:600;" }
    dataKwW = textWidth ctx.charWidth kw
    nameNode = LText
      { x: 8.0 + dataKwW, y: headerH / 2.0, text: name
      , fontSize: ctx.fontSize, style: "fill:" <> adtColors.headerText <> ";font-weight:700;" }
    paramStartX = 8.0 + dataKwW + textWidth ctx.charWidth name + 6.0
    paramAcc = Array.foldl (\acc p ->
      let pillR = renderSmallPill ctxV acc.px 6.0 p (headerH / 2.0)
      in { px: acc.px + pillR.width + 3.0, nodes: acc.nodes <> pillR.nodes }
    ) { px: paramStartX, nodes: [] } typeParams

    -- Vertical rail
    railTop = ctorAreaTop
    railBottom = ctorAreaTop + toNumber (ctorCount - 1) * adtRowHeight
    railLine = if ctorCount > 1 then
      [ LLine { x1: adtRailX, y1: railTop + adtRowHeight / 2.0
              , x2: adtRailX, y2: railBottom + adtRowHeight / 2.0
              , stroke: adtColors.rail, strokeWidth: 2.0
              , strokeLinecap: "round", strokeDasharray: "" } ]
    else []

    -- Constructor branches
    ctorNodes = Array.foldl (\acc idx ->
      case Array.index constructors idx of
        Nothing -> acc
        Just ctor ->
          let
            cy = ctorAreaTop + toNumber idx * adtRowHeight + adtRowHeight / 2.0
            branch = LLine
              { x1: adtRailX, y1: cy, x2: adtCtorX - 4.0, y2: cy
              , stroke: adtColors.ctorBranch, strokeWidth: 1.5
              , strokeLinecap: "round", strokeDasharray: "" }
            dot = LCircle
              { cx: adtRailX, cy, r: 3.0
              , fill: "white", stroke: adtColors.rail, strokeWidth: 1.5 }
            nameN = LText
              { x: adtCtorX, y: cy, text: ctor.name
              , fontSize: ctx.fontSize
              , style: "fill:" <> adtColors.ctorName <> ";font-weight:700;" }
            argsR = Array.foldl (\a argIdx ->
              case Array.index ctor.args argIdx of
                Nothing -> a
                Just ast ->
                  let
                    spacing = if argIdx > 0 then ctx.charWidth else 0.0
                    ax = a.ax + spacing
                    argR = renderNode ctxV ax (cy - ctx.lineHeight / 2.0) ast
                  in { ax: ax + argR.width, nodes: a.nodes <> argR.nodes }
            ) { ax: argX, nodes: [] } (Array.range 0 (Array.length ctor.args - 1))
            noArgDash = if Array.null ctor.args then
              [ LLine { x1: argX, y1: cy, x2: argX + 12.0, y2: cy
                       , stroke: "#ddd", strokeWidth: 1.0
                       , strokeLinecap: "", strokeDasharray: "2 2" } ]
            else []
          in acc <> [branch, dot, nameN] <> argsR.nodes <> noArgDash
    ) [] (Array.range 0 (ctorCount - 1))

    layout = LGroup
      { transform: ""
      , children: [headerRect, dataKw, nameNode] <> paramAcc.nodes <> railLine <> ctorNodes
      }
  in
    { layout, dimensions: { width: totalW, height: totalH } }

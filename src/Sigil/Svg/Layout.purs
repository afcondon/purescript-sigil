-- | Core layout functions: pure RenderType → LayoutNode conversion.
-- |
-- | All functions are pure — they produce positioned LayoutNode trees
-- | without touching the DOM.
module Sigil.Svg.Layout
  ( unwrapType
  , parseClassName
  , renderNode
  , renderSmallPill
  , renderSigletVarDot
  , renderConstraintPile
  ) where

import Prelude
import Prim hiding (Constraint, Row)

import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (split, trim)
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))

import Sigil.Types (RenderType(..), RowField, Constraint)
import Sigil.Svg.Types (LayoutNode(..), RenderResult)
import Sigil.Color (colors, isEffectName)
import Sigil.Text (constraintText, collectArrowParams, renderTypeToText)
import Sigil.Svg.Measure (RenderContext, textWidth, sigletDotWidth, measure, measureFieldFull)

-- ============================================================
-- Unwrap / parse helpers
-- ============================================================

-- | Peel off outer forall and constraint layers.
unwrapType :: RenderType -> { forallVars :: Array String, constraints :: Array Constraint, body :: RenderType }
unwrapType = go [] []
  where
  go fvs cs = case _ of
    TForall vars body -> go (fvs <> vars) cs body
    TConstrained constraints body -> go fvs (cs <> constraints) body
    body -> { forallVars: fvs, constraints: cs, body }

-- | Parse "Foldable f" → { name: "Foldable", params: ["f"] }.
parseClassName :: String -> Maybe { name :: String, params :: Array String }
parseClassName s =
  let parts = Array.filter (_ /= "") (split (Pattern " ") (trim s))
  in case Array.uncons parts of
    Nothing -> Nothing
    Just { head, tail } -> Just { name: head, params: tail }

-- ============================================================
-- Node helpers
-- ============================================================

isTypeVarNode :: RenderType -> Boolean
isTypeVarNode = case _ of
  TVar _ -> true
  _ -> false

-- ============================================================
-- Primitive renderers
-- ============================================================

-- | Type variable pill (colored rounded rect + white text).
renderVarPill :: RenderContext -> Number -> Number -> String -> Number -> RenderResult
renderVarPill ctx x y name fontSize =
  let
    color = fromMaybe colors.typevar (Map.lookup name ctx.varColors)
    pw = textWidth ctx.charWidth name + 10.0
    ph = fontSize + 5.0
    py = y + ctx.lineHeight / 2.0 - ph / 2.0
  in
    { nodes:
      [ LRect { x, y: py, width: pw, height: ph, rx: 3.0
              , style: "fill:" <> color <> ";" }
      , LText { x: x + 5.0, y: y + ctx.lineHeight / 2.0, text: name
              , fontSize, style: "fill:white;font-weight:600;font-style:italic;" }
      ]
    , width: pw
    }

-- | Small variable pill for annotations (forall row, class row).
renderSmallPill :: RenderContext -> Number -> Number -> String -> Number -> RenderResult
renderSmallPill ctx x _y name centerY =
  let
    color = fromMaybe colors.typevar (Map.lookup name ctx.varColors)
    pw = textWidth ctx.charWidth name + 8.0
    ph = 16.0
    py = centerY - ph / 2.0
  in
    { nodes:
      [ LRect { x, y: py, width: pw, height: ph, rx: 3.0
              , style: "fill:" <> color <> ";" }
      , LText { x: x + 4.0, y: centerY, text: name
              , fontSize: 11.0, style: "fill:white;font-weight:600;font-style:italic;" }
      ]
    , width: pw
    }

-- | Arrow glyph between function parameters.
renderArrow :: RenderContext -> Number -> Number -> RenderResult
renderArrow ctx x y =
  let arrowW = ctx.charWidth * 3.5
  in { nodes:
      [ LText { x: x + ctx.charWidth, y: y + ctx.lineHeight / 2.0
              , text: "\x2192", fontSize: 16.0
              , style: "fill:" <> colors.arrow <> ";font-weight:400;" }
      ]
     , width: arrowW
     }

-- | Siglet dot: small circle replacing a concrete type name.
-- | Effect types get a filled purple circle; others get a hollow green circle.
renderSigletDot :: RenderContext -> Number -> Number -> String -> RenderResult
renderSigletDot ctx x y name =
  let
    r = ctx.fontSize * 0.3
    dotW = sigletDotWidth ctx.fontSize
    cx = x + dotW / 2.0
    cy = y + ctx.lineHeight / 2.0
    isEffect = isEffectName name
  in
    { nodes:
      [ LCircle
        { cx, cy, r
        , fill: if isEffect then colors.effect else "none"
        , stroke: if isEffect then "none" else colors.constructor
        , strokeWidth: if isEffect then 0.0 else 1.5
        }
      ]
    , width: dotW
    }

-- | Siglet var dot: filled colored circle for multi-letter type variables.
renderSigletVarDot :: RenderContext -> Number -> Number -> String -> RenderResult
renderSigletVarDot ctx x y name =
  let
    r = ctx.fontSize * 0.3
    dotW = sigletDotWidth ctx.fontSize
    cx = x + dotW / 2.0
    cy = y + ctx.lineHeight / 2.0
    color = fromMaybe colors.typevar (Map.lookup name ctx.varColors)
  in
    { nodes: [LCircle { cx, cy, r, fill: color, stroke: "none", strokeWidth: 0.0 }]
    , width: dotW
    }

-- | Constraint pile (stacked rounded rects with dashed connector).
-- | In siglet mode, class names become dots and args use renderNode
-- | (so multi-letter vars → dots, single-letter → pills, TCon → dots).
renderConstraintPile :: RenderContext -> Number -> Number -> Array Constraint -> RenderResult
renderConstraintPile ctx x y constraints =
  let
    pileNodes = Array.foldl (\acc idx ->
      case Array.index constraints idx of
        Nothing -> acc
        Just c ->
          let pillY = y + toNumber idx * 24.0
          in acc <> renderOneConstraint ctx x pillY c
    ) [] (Array.range 0 (Array.length constraints - 1))

    pileBottom = y + toNumber (Array.length constraints) * 24.0
    dashLine = LLine
      { x1: x + 4.0, y1: pileBottom
      , x2: x + 4.0, y2: pileBottom + 8.0
      , stroke: colors.constraintBd, strokeWidth: 1.0
      , strokeLinecap: "", strokeDasharray: "2 2"
      }
  in
    { nodes: pileNodes <> [dashLine], width: 0.0 }

-- | Render a single constraint pill. In siglet mode, the class name is a
-- | hollow dot and each argument goes through renderNode for dot/pill treatment.
renderOneConstraint :: RenderContext -> Number -> Number -> Constraint -> Array LayoutNode
renderOneConstraint ctx x pillY c
  | ctx.sigletMode =
      let
        nameR = renderSigletDot ctx (x + 6.0) pillY c.className
        argsAcc = Array.foldl (\a arg ->
          let argR = renderNode ctx a.cx pillY arg
          in { cx: a.cx + argR.width + 2.0, nodes: a.nodes <> argR.nodes }
        ) { cx: x + 6.0 + nameR.width + 2.0, nodes: [] } c.args
        totalW = argsAcc.cx - x + 4.0
      in
        [ LRect { x, y: pillY, width: totalW, height: 20.0, rx: 3.0
                , style: "fill:" <> colors.constraintBg <> ";stroke:" <> colors.constraintBd <> ";stroke-width:1;" }
        ] <> nameR.nodes <> argsAcc.nodes
  | otherwise =
      let
        text = constraintText c
        w = textWidth ctx.charWidth text + 16.0
      in
        [ LRect { x, y: pillY, width: w, height: 20.0, rx: 3.0
                , style: "fill:" <> colors.constraintBg <> ";stroke:" <> colors.constraintBd <> ";stroke-width:1;" }
        , LText { x: x + 8.0, y: pillY + 10.0, text
                , fontSize: 11.0, style: "fill:" <> colors.constraint <> ";font-weight:600;" }
        ]

-- ============================================================
-- Main recursive renderer
-- ============================================================

-- | Lay out a RenderType node at position (x, y). Returns positioned
-- | LayoutNodes and the horizontal width consumed.
renderNode :: RenderContext -> Number -> Number -> RenderType -> RenderResult
renderNode ctx x y = case _ of
  TVar name ->
    if ctx.sigletMode && SCU.length name > 1
      then renderSigletVarDot ctx x y name
      else renderVarPill ctx x y name ctx.fontSize

  TCon name ->
    if ctx.sigletMode then renderSigletDot ctx x y name
    else
      let col = if isEffectName name then colors.effect else colors.constructor
      in { nodes:
            [ LText { x, y: y + ctx.lineHeight / 2.0, text: name
                    , fontSize: ctx.fontSize
                    , style: "fill:" <> col <> ";font-weight:600;" } ]
         , width: textWidth ctx.charWidth name
         }

  -- Collapse Record (row) → record table
  TApp (TCon "Record") [TRow fields rowVar] ->
    renderRecord ctx x y fields rowVar false

  TApp head args ->
    renderApplied ctx x y head args

  TArrow from to ->
    renderFunction ctx x y (collectArrowParams (TArrow from to))

  TRecord fields rowVar ->
    renderRecord ctx x y fields rowVar false

  TRow fields rowVar ->
    renderRecord ctx x y fields rowVar true

  TForall vars body ->
    renderInlineForall ctx x y vars body

  TConstrained cs body ->
    let
      cH = toNumber (Array.length cs) * 24.0 + 10.0
      pileR = renderConstraintPile ctx x y cs
      bodyR = renderNode ctx x (y + cH) body
    in { nodes: pileR.nodes <> bodyR.nodes, width: bodyR.width }

  TParens inner ->
    renderParens ctx x y inner

  TOperator l op r ->
    renderOperator ctx x y l op r

  TKinded ty kind ->
    renderKinded ctx x y ty kind

  TString s ->
    let text = "\"" <> s <> "\""
    in { nodes:
          [ LText { x, y: y + ctx.lineHeight / 2.0, text
                  , fontSize: ctx.fontSize
                  , style: "fill:" <> colors.constructor <> ";font-weight:600;" } ]
       , width: textWidth ctx.charWidth text
       }

  TWildcard ->
    renderVarPill ctx x y "_" ctx.fontSize

-- ============================================================
-- Composite renderers
-- ============================================================

renderApplied :: RenderContext -> Number -> Number -> RenderType -> Array RenderType -> RenderResult
renderApplied ctx x y head args =
  let
    isHKT = isTypeVarNode head
    totalW = (measure ctx (TApp head args)).width

    hktBox = if isHKT then
      [ LRect { x: x - 2.0, y: y - 1.0, width: totalW + 4.0, height: ctx.lineHeight + 2.0
              , rx: 3.0
              , style: "fill:" <> colors.hktBg <> ";stroke:" <> colors.hktBd <> ";stroke-width:1;" } ]
    else []

    -- Render head recursively (handles nested TApp, TParens, etc.)
    headR = renderNode ctx x y head

    argsAcc = Array.foldl (\acc arg ->
      let
        cx = acc.cx + ctx.charWidth
        argR = renderNode ctx cx y arg
      in { cx: cx + argR.width, nodes: acc.nodes <> argR.nodes }
    ) { cx: x + headR.width, nodes: [] } args
  in
    { nodes: hktBox <> headR.nodes <> argsAcc.nodes
    , width: argsAcc.cx - x
    }

renderFunction :: RenderContext -> Number -> Number -> Array RenderType -> RenderResult
renderFunction ctx x y parts =
  case Array.unsnoc parts of
    Nothing -> { nodes: [], width: 0.0 }
    Just { init: params, last: ret } ->
      let
        paramsAcc = Array.foldl (\acc p ->
          let
            pR = renderNode ctx acc.cx y p
            aR = renderArrow ctx (acc.cx + pR.width) y
          in { cx: acc.cx + pR.width + aR.width
             , nodes: acc.nodes <> pR.nodes <> aR.nodes
             }
        ) { cx: x, nodes: [] } params
        retR = renderNode ctx paramsAcc.cx y ret
      in { nodes: paramsAcc.nodes <> retR.nodes
         , width: paramsAcc.cx + retR.width - x
         }

renderInlineForall :: RenderContext -> Number -> Number -> Array String -> RenderType -> RenderResult
renderInlineForall ctx x y vars body =
  let
    pad = 5.0
    cx0 = x + pad
    forallNode = LText
      { x: cx0, y: y + ctx.lineHeight / 2.0, text: "\x2200"
      , fontSize: 12.0, style: "fill:" <> colors.keyword <> ";font-weight:700;" }
    cx1 = cx0 + ctx.charWidth + 2.0

    varsAcc = Array.foldl (\acc v ->
      let
        cx = if acc.first then acc.cx else acc.cx + 2.0
        pillR = if ctx.sigletMode && SCU.length v > 1
          then renderSigletVarDot ctx cx y v
          else renderVarPill ctx cx y v 11.0
      in { cx: cx + pillR.width + 1.0, nodes: acc.nodes <> pillR.nodes, first: false }
    ) { cx: cx1, nodes: [], first: true } vars

    dotNode = LText
      { x: varsAcc.cx, y: y + ctx.lineHeight / 2.0, text: "."
      , fontSize: 12.0, style: "fill:" <> colors.keyword <> ";font-weight:600;" }
    cx2 = varsAcc.cx + ctx.charWidth
    bodyR = renderNode ctx cx2 y body
    cx3 = cx2 + bodyR.width + pad
    totalW = cx3 - x
    outline = LRect
      { x, y: y - 2.0, width: totalW, height: ctx.lineHeight + 4.0, rx: 4.0
      , style: "fill:rgba(123,97,255,0.04);stroke:#7b61ff;stroke-width:1;stroke-dasharray:3 2;" }
  in
    { nodes: [forallNode] <> varsAcc.nodes <> [dotNode] <> bodyR.nodes <> [outline]
    , width: totalW
    }

renderParens :: RenderContext -> Number -> Number -> RenderType -> RenderResult
renderParens ctx x y inner =
  let
    openP = LText { x, y: y + ctx.lineHeight / 2.0, text: "("
                   , fontSize: ctx.fontSize, style: "fill:" <> colors.paren <> ";" }
    innerR = renderNode ctx (x + ctx.charWidth) y inner
    closeP = LText { x: x + ctx.charWidth + innerR.width, y: y + ctx.lineHeight / 2.0
                   , text: ")", fontSize: ctx.fontSize, style: "fill:" <> colors.paren <> ";" }
  in
    { nodes: [openP] <> innerR.nodes <> [closeP]
    , width: ctx.charWidth + innerR.width + ctx.charWidth
    }

renderOperator :: RenderContext -> Number -> Number -> RenderType -> String -> RenderType -> RenderResult
renderOperator ctx x y l op r =
  let
    lR = renderNode ctx x y l
    opText = " " <> op <> " "
    opNode = LText
      { x: x + lR.width, y: y + ctx.lineHeight / 2.0, text: opText
      , fontSize: ctx.fontSize, style: "fill:" <> colors.arrow <> ";font-weight:600;" }
    opW = textWidth ctx.charWidth opText
    rR = renderNode ctx (x + lR.width + opW) y r
  in
    { nodes: lR.nodes <> [opNode] <> rR.nodes
    , width: lR.width + opW + rR.width
    }

renderKinded :: RenderContext -> Number -> Number -> RenderType -> RenderType -> RenderResult
renderKinded ctx x y ty kind =
  let
    tyR = renderNode ctx x y ty
    sepW = textWidth ctx.charWidth " :: "
    sepNode = LText
      { x: x + tyR.width, y: y + ctx.lineHeight / 2.0, text: " :: "
      , fontSize: ctx.fontSize, style: "fill:" <> colors.separator <> ";" }
    kindR = renderNode ctx (x + tyR.width + sepW) y kind
  in
    { nodes: tyR.nodes <> [sepNode] <> kindR.nodes
    , width: tyR.width + sepW + kindR.width
    }

-- ============================================================
-- Record / row table
-- ============================================================

renderRecord :: RenderContext -> Number -> Number -> Array RowField -> Maybe String -> Boolean -> RenderResult
renderRecord ctx x y fields rowVar isRowType =
  let isOpen = case rowVar of
        Just _ -> true
        Nothing -> false
  in
  if Array.null fields && not isOpen then
    let label = if isRowType then "()" else "{}"
    in { nodes:
          [ LText { x, y: y + ctx.lineHeight / 2.0, text: label
                  , fontSize: ctx.fontSize, style: "fill:" <> colors.paren <> ";" } ]
       , width: textWidth ctx.charWidth label
       }
  else
    let
      fieldStats = map (\f ->
        let
          nameW = textWidth ctx.charWidth f.label
          ftm = measureFieldFull ctx f.value
          rowH = max (ctx.lineHeight + 4.0) (ftm.height + 6.0)
        in { nameW, typeW: ftm.width, rowH }
      ) fields

      maxNameW = Array.foldl (\acc s -> max acc s.nameW) 0.0 fieldStats
      maxTypeW = Array.foldl (\acc s -> max acc s.typeW) 0.0 fieldStats
      rowHeights = map _.rowH fieldStats
      colSepW = textWidth ctx.charWidth " :: "
      tableW = max (ctx.padding.x * 2.0 + maxNameW + colSepW + maxTypeW + ctx.padding.x) 80.0
      totalRowH = Array.foldl (+) 0.0 rowHeights
      rowVarH = if isOpen then 28.0 else 0.0
      tableH = totalRowH + rowVarH + ctx.padding.y * 2.0

      strokeW = if isOpen then "1.5" else "1"
      dashStyle = if isOpen then "stroke-dasharray:5 3;" else ""
      borderRect = LRect
        { x, y, width: tableW, height: tableH, rx: 4.0
        , style: "fill:white;stroke:" <> colors.recordBorder <> ";stroke-width:" <> strokeW <> ";" <> dashStyle }

      fieldNodes = Array.foldl (\acc idx ->
        case Array.index fields idx of
          Nothing -> acc
          Just field ->
            let
              rh = fromMaybe ctx.lineHeight (Array.index rowHeights idx)
              currentY = acc.currentY
              separator = if idx > 0 then
                [ LLine { x1: x + 4.0, y1: currentY - 2.0
                         , x2: x + tableW - 4.0, y2: currentY - 2.0
                         , stroke: "#f0f0f0", strokeWidth: 1.0
                         , strokeLinecap: "", strokeDasharray: "" } ]
              else []
              nameNode = LText
                { x: x + ctx.padding.x, y: currentY + ctx.lineHeight / 2.0
                , text: field.label, fontSize: 12.0
                , style: "fill:" <> colors.fieldName <> ";font-weight:600;" }
              sepNode = LText
                { x: x + ctx.padding.x + maxNameW + 4.0, y: currentY + ctx.lineHeight / 2.0
                , text: "::", fontSize: 12.0, style: "fill:" <> colors.separator <> ";" }
              typeR = renderFieldNode ctx (x + ctx.padding.x + maxNameW + colSepW) currentY field.value
            in { currentY: currentY + rh
               , nodes: acc.nodes <> separator <> [nameNode, sepNode] <> typeR.nodes
               }
      ) { currentY: y + ctx.padding.y, nodes: [] }
        (Array.range 0 (Array.length fields - 1))

      rowVarNodes = case rowVar of
        Just rv ->
          let
            ellY = fieldNodes.currentY + 4.0
            cx = x + tableW / 2.0
            rvColor = fromMaybe colors.rowVar (Map.lookup rv ctx.varColors)
            dots = map (\d ->
              LCircle { cx, cy: ellY + toNumber d * 6.0 + 3.0, r: 1.5
                       , fill: rvColor, stroke: "", strokeWidth: 0.0 }
            ) [0, 1, 2]
            rvW = textWidth ctx.charWidth rv + 8.0
            rvX = cx + 10.0
            rvPill = LRect
              { x: rvX, y: ellY + 3.0, width: rvW, height: 14.0, rx: 3.0
              , style: "fill:" <> rvColor <> ";" }
            rvText = LText
              { x: rvX + 4.0, y: ellY + 10.0, text: rv
              , fontSize: 10.0, style: "fill:white;font-style:italic;font-weight:600;" }
          in dots <> [rvPill, rvText]
        Nothing -> []
    in
      { nodes: [borderRect] <> fieldNodes.nodes <> rowVarNodes
      , width: tableW
      }

-- ============================================================
-- Field-type renderer (smaller font/pills inside record tables)
-- ============================================================

renderFieldNode :: RenderContext -> Number -> Number -> RenderType -> RenderResult
renderFieldNode ctx x y = case _ of
  TRecord fields rowVar ->
    renderRecord ctx x y fields rowVar false

  TRow fields rowVar ->
    renderRecord ctx x y fields rowVar true

  TVar name ->
    if ctx.sigletMode && SCU.length name > 1
      then renderSigletVarDot ctx x y name
      else
        let
          color = fromMaybe colors.typevar (Map.lookup name ctx.varColors)
          pw = textWidth ctx.charWidth name + 8.0
          ph = 15.0
          py = y + ctx.lineHeight / 2.0 - ph / 2.0
        in
          { nodes:
            [ LRect { x, y: py, width: pw, height: ph, rx: 3.0
                    , style: "fill:" <> color <> ";" }
            , LText { x: x + 4.0, y: y + ctx.lineHeight / 2.0, text: name
                    , fontSize: 11.0, style: "fill:white;font-weight:600;font-style:italic;" } ]
          , width: pw
          }

  TCon name ->
    if ctx.sigletMode then renderSigletDot ctx x y name
    else
      let col = if isEffectName name then colors.effect else colors.constructor
      in { nodes:
            [ LText { x, y: y + ctx.lineHeight / 2.0, text: name
                    , fontSize: 12.0, style: "fill:" <> col <> ";font-weight:600;" } ]
         , width: textWidth ctx.charWidth name
         }

  -- Collapse Record (row) → record table in field context
  TApp (TCon "Record") [TRow fields rowVar] ->
    renderRecord ctx x y fields rowVar false

  TApp head args ->
    let
      -- Render head recursively (handles nested TApp, TParens, etc.)
      headR = renderFieldNode ctx x y head
      argsAcc = Array.foldl (\acc arg ->
        let
          cx = acc.cx + ctx.charWidth * 0.8
          argR = renderFieldNode ctx cx y arg
          argW = (measureFieldFull ctx arg).width
        in { cx: cx + argW, nodes: acc.nodes <> argR.nodes }
      ) { cx: x + headR.width, nodes: [] } args
    in
      { nodes: headR.nodes <> argsAcc.nodes
      , width: argsAcc.cx - x
      }

  TParens inner ->
    let
      innerR = renderFieldNode ctx (x + ctx.charWidth) y inner
      innerW = (measureFieldFull ctx inner).width
    in
      { nodes:
        [ LText { x, y: y + ctx.lineHeight / 2.0, text: "("
                , fontSize: 12.0, style: "fill:" <> colors.paren <> ";" } ]
        <> innerR.nodes <>
        [ LText { x: x + ctx.charWidth + innerW, y: y + ctx.lineHeight / 2.0, text: ")"
                , fontSize: 12.0, style: "fill:" <> colors.paren <> ";" } ]
      , width: ctx.charWidth + innerW + ctx.charWidth
      }

  TArrow from to ->
    let
      parts = collectArrowParams (TArrow from to)
      arrowW = ctx.charWidth * 3.5
    in case Array.unsnoc parts of
      Nothing -> { nodes: [], width: 0.0 }
      Just { init: params, last: ret } ->
        let
          paramsAcc = Array.foldl (\acc p ->
            let
              pR = renderFieldNode ctx acc.cx y p
              pW = (measureFieldFull ctx p).width
              arrowNode = LText
                { x: acc.cx + pW + ctx.charWidth * 0.5, y: y + ctx.lineHeight / 2.0
                , text: "\x2192", fontSize: 12.0
                , style: "fill:" <> colors.arrow <> ";" }
            in { cx: acc.cx + pW + arrowW
               , nodes: acc.nodes <> pR.nodes <> [arrowNode]
               }
          ) { cx: x, nodes: [] } params
          retR = renderFieldNode ctx paramsAcc.cx y ret
        in { nodes: paramsAcc.nodes <> retR.nodes
           , width: paramsAcc.cx + retR.width - x
           }

  other ->
    let text = renderTypeToText other
    in { nodes:
          [ LText { x, y: y + ctx.lineHeight / 2.0, text
                  , fontSize: 12.0, style: "fill:" <> colors.fieldType <> ";" } ]
       , width: textWidth ctx.charWidth text
       }

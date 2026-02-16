-- | Pure measurement functions for type signature layout.
-- |
-- | Computes dimensions (width, height) for any RenderType node
-- | using character-width arithmetic. No DOM access.
module Sigil.Svg.Measure
  ( textWidth
  , varPillWidth
  , sigletDotWidth
  , measure
  , measureTable
  , measureFieldFull
  , RenderContext
  , defaultRenderContext
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU

import Sigil.Types (RenderType(..), RowField)
import Sigil.Svg.Types (Dimensions)
import Sigil.Text (collectArrowParams, renderTypeToText)

-- | Rendering context threaded through layout functions.
type RenderContext =
  { varColors :: Map String String
  , fontSize :: Number
  , charWidth :: Number
  , lineHeight :: Number
  , padding :: { x :: Number, y :: Number }
  , sigletMode :: Boolean
  }

defaultRenderContext :: RenderContext
defaultRenderContext =
  { varColors: Map.empty
  , fontSize: 13.0
  , charWidth: 7.8
  , lineHeight: 20.0
  , padding: { x: 8.0, y: 6.0 }
  , sigletMode: false
  }

-- | Fixed width of a siglet dot (TCon rendered as circle).
sigletDotWidth :: Number -> Number
sigletDotWidth fontSize = fontSize * 0.6 + 4.0

-- | Width of a text string in the monospace font.
textWidth :: Number -> String -> Number
textWidth charWidth str = toNumber (SCU.length str) * charWidth

-- | Width of a type variable pill.
varPillWidth :: Number -> String -> Number
varPillWidth charWidth name = textWidth charWidth name + 10.0

-- | Measure a RenderType node's dimensions.
measure :: RenderContext -> RenderType -> Dimensions
measure ctx = case _ of
  TVar name ->
    if ctx.sigletMode && SCU.length name > 1
      then { width: sigletDotWidth ctx.fontSize, height: ctx.lineHeight }
      else { width: varPillWidth ctx.charWidth name, height: ctx.lineHeight }

  TCon name ->
    if ctx.sigletMode
      then { width: sigletDotWidth ctx.fontSize, height: ctx.lineHeight }
      else { width: textWidth ctx.charWidth name, height: ctx.lineHeight }

  -- Collapse Record (row) → record table
  TApp (TCon "Record") [TRow fields rowVar] ->
    measureTable ctx fields rowVar

  TApp head args ->
    let
      headW = (measure ctx head).width
      result = Array.foldl (\acc a ->
        let am = measure ctx a
        in { w: acc.w + ctx.charWidth + am.width
           , maxH: max acc.maxH am.height
           }
      ) { w: 0.0, maxH: ctx.lineHeight } args
    in { width: headW + result.w, height: result.maxH }

  TArrow from to ->
    let
      parts = collectArrowParams (TArrow from to)
      arrowW = ctx.charWidth * 3.5
    in case Array.unsnoc parts of
      Nothing -> { width: 0.0, height: ctx.lineHeight }
      Just { init: params, last: ret } ->
        let
          paramsW = Array.foldl (\w p -> w + (measure ctx p).width + arrowW) 0.0 params
          retM = measure ctx ret
          maxH = Array.foldl (\h p -> max h (measure ctx p).height) retM.height params
        in { width: paramsW + retM.width, height: max ctx.lineHeight maxH }

  TRecord fields rowVar -> measureTable ctx fields rowVar
  TRow fields rowVar -> measureTable ctx fields rowVar

  TForall vars body ->
    let
      w0 = 10.0 -- 5px padding each side
      w1 = w0 + ctx.charWidth + 2.0 -- forall symbol
      w2 = Array.foldl (\w v ->
        let vw = if ctx.sigletMode && SCU.length v > 1
              then sigletDotWidth ctx.fontSize
              else varPillWidth ctx.charWidth v
        in w + vw + 3.0) w1 vars
      w3 = w2 + ctx.charWidth -- dot separator
      bodyM = measure ctx body
    in { width: w3 + bodyM.width, height: bodyM.height }

  TConstrained constraints body ->
    let
      bodyM = measure ctx body
      cH = toNumber (Array.length constraints) * 24.0 + 10.0
    in { width: bodyM.width, height: bodyM.height + cH }

  TParens inner ->
    let m = measure ctx inner
    in { width: m.width + 2.0 * ctx.charWidth, height: m.height }

  TOperator l op r ->
    let
      lW = (measure ctx l).width
      rW = (measure ctx r).width
    in { width: lW + textWidth ctx.charWidth (" " <> op <> " ") + rW, height: ctx.lineHeight }

  TKinded ty kind ->
    let
      tW = (measure ctx ty).width
      kW = (measure ctx kind).width
    in { width: tW + textWidth ctx.charWidth " :: " + kW, height: ctx.lineHeight }

  TString s ->
    { width: textWidth ctx.charWidth ("\"" <> s <> "\""), height: ctx.lineHeight }

  TWildcard ->
    { width: varPillWidth ctx.charWidth "_", height: ctx.lineHeight }

-- | Measure a record/row table.
measureTable :: RenderContext -> Array RowField -> Maybe String -> Dimensions
measureTable ctx fields rowVar =
  if Array.null fields then
    case rowVar of
      Just _ -> { width: 80.0, height: 50.0 }
      Nothing -> { width: 40.0, height: 30.0 }
  else
    let
      result = Array.foldl (\acc f ->
        let
          nameW = textWidth ctx.charWidth f.label
          ftm = measureFieldFull ctx f.value
          rowH = max (ctx.lineHeight + 4.0) (ftm.height + 6.0)
        in { maxNameW: max acc.maxNameW nameW
           , maxTypeW: max acc.maxTypeW ftm.width
           , totalRowH: acc.totalRowH + rowH
           }
      ) { maxNameW: 0.0, maxTypeW: 0.0, totalRowH: 0.0 } fields
      colSepW = textWidth ctx.charWidth " :: "
      tableW = ctx.padding.x * 2.0 + result.maxNameW + colSepW + result.maxTypeW + ctx.padding.x
      rowVarH = case rowVar of
        Just _ -> 28.0
        Nothing -> 0.0
    in { width: max tableW 80.0
       , height: result.totalRowH + rowVarH + ctx.padding.y * 2.0
       }

-- | Measure a field type (used inside record tables). Handles nested
-- | records and uses full measure for applied types.
measureFieldFull :: RenderContext -> RenderType -> Dimensions
measureFieldFull ctx = case _ of
  TVar name ->
    if ctx.sigletMode && SCU.length name > 1
      then { width: sigletDotWidth ctx.fontSize, height: ctx.lineHeight }
      else { width: varPillWidth ctx.charWidth name, height: ctx.lineHeight }

  TCon name ->
    if ctx.sigletMode
      then { width: sigletDotWidth ctx.fontSize, height: ctx.lineHeight }
      else { width: textWidth ctx.charWidth name, height: ctx.lineHeight }

  TRecord fields rowVar -> measureTable ctx fields rowVar
  TRow fields rowVar -> measureTable ctx fields rowVar

  -- Collapse Record (row) → record table
  TApp (TCon "Record") [TRow fields rowVar] ->
    measureTable ctx fields rowVar

  TApp head args ->
    let
      headW = (measure ctx head).width
      result = Array.foldl (\acc a ->
        let am = measureFieldFull ctx a
        in { w: acc.w + ctx.charWidth + am.width
           , maxH: max acc.maxH am.height
           }
      ) { w: 0.0, maxH: ctx.lineHeight } args
    in { width: headW + result.w, height: result.maxH }

  TParens inner ->
    let m = measureFieldFull ctx inner
    in { width: m.width + 2.0 * ctx.charWidth, height: m.height }

  TArrow from to ->
    let
      parts = collectArrowParams (TArrow from to)
      arrowW = ctx.charWidth * 3.5
    in case Array.unsnoc parts of
      Nothing -> { width: 0.0, height: ctx.lineHeight }
      Just { init: params, last: ret } ->
        let
          paramsW = Array.foldl (\w p -> w + (measureFieldFull ctx p).width + arrowW) 0.0 params
          retW = (measureFieldFull ctx ret).width
        in { width: paramsW + retW, height: ctx.lineHeight }

  other ->
    { width: textWidth ctx.charWidth (renderTypeToText other), height: ctx.lineHeight }

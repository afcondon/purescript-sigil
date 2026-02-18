-- | HTML string rendering of type signatures with semantic markup.
-- |
-- | Maps PureScript type concepts to semantic HTML elements:
-- |   TVar       → <var>    (mathematical variable)
-- |   TCon       → <code>   (type constructor / identifier)
-- |   TArrow     → <ol>     (ordered parameter list)
-- |   TRecord    → <dl>     (definition list: field → type)
-- |   TForall    → <var>    (quantified variables)
-- |   Constraint → <em>     (qualifying emphasis)
-- |   separators → <small>  (::, →, ., parens — decorative punctuation)
-- |   name       → <dfn>    (term being defined)
-- |
-- | Visual appearance is entirely CSS-driven, enabling alternate renderings
-- | of the same semantic document.
module Sigil.Html
  ( renderSignature
  , renderBody
  , renderSiglet
  , renderSignet
  , renderDataDecl
  , renderClassDecl
  , renderTypeSynonym
  , renderForeignImport
  , renderSignatureInto
  , renderBodyInto
  , renderSigletInto
  , renderSignetInto
  , renderDataDeclInto
  , renderClassDeclInto
  , renderTypeSynonymInto
  , renderForeignImportInto
  , peelSignature
  , PeeledSig
  , escapeHtml
  ) where

import Prelude
import Prim hiding (Constraint, Row)

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as SCU
import Effect (Effect)

import Sigil.Color (assignVarColors, isEffectName)
import Sigil.Text (collectTypeVars, collectArrowParams, collectForallVars)
import Sigil.Types (RenderType(..), Constraint, SuperclassInfo)

-- =============================================================================
-- FFI
-- =============================================================================

foreign import _renderInto :: String -> String -> Effect Unit

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render a full-size type signature to an HTML string.
-- | Uses Swiss typographic layout: preamble (∀ left, constraints right),
-- | declaration (name ::), body (indented with left rule).
renderSignature
  :: { name :: String, ast :: RenderType, typeParams :: Array String, className :: Maybe String }
  -> String
renderSignature { name, ast, typeParams, className } =
  let
    peeled = peelSignature ast
    allVars = Set.toUnfoldable (collectTypeVars ast) <> peeled.forallVars <> typeParams
    varColors = assignVarColors (Array.nub allVars)
    ctx = { varColors, constrainedVars: Set.empty }
    hasPreamble = not (Array.null peeled.forallVars) || not (Array.null peeled.constraints)
  in
  "<code class=\"sig-full\">"
    <> (if hasPreamble then renderPreamble ctx peeled else "")
    <> "<div class=\"sig-decl\">"
        <> "<dfn class=\"sig-name\">" <> escapeHtml name <> "</dfn>"
        <> case className of
             Just cn -> "<small class=\"sig-sep\">" <> escapeHtml (" (" <> cn <> ")") <> "</small>"
             Nothing -> ""
        <> "<small class=\"sig-dcolon\"> ::</small>"
      <> "</div>"
    <> "<div class=\"sig-body\">"
        <> renderTypeTree ctx peeled.body
      <> "</div>"
  <> "</code>"

-- | Render the sigil body only (no name header, no ::) to HTML.
-- | Used for inline rendering in value cells when there's enough space.
-- | Still shows preamble (quantifier + constraints) if present.
renderBody :: { ast :: RenderType } -> String
renderBody { ast } =
  let
    peeled = peelSignature ast
    allVars = Set.toUnfoldable (collectTypeVars ast) <> peeled.forallVars
    varColors = assignVarColors (Array.nub allVars)
    ctx = { varColors, constrainedVars: Set.empty }
    hasPreamble = not (Array.null peeled.forallVars) || not (Array.null peeled.constraints)
  in
  "<code class=\"sig-full sig-body-only\">"
    <> (if hasPreamble then renderPreamble ctx peeled else "")
    <> "<div class=\"sig-body\">"
        <> renderTypeTree ctx peeled.body
      <> "</div>"
  <> "</code>"

-- | Render a siglet (miniature signature) to HTML.
-- | Constrained type variables get pill style; unconstrained get colored text.
renderSiglet :: { ast :: RenderType } -> String
renderSiglet { ast } =
  let
    peeled = peelSignature ast
    allVars = Set.toUnfoldable (collectTypeVars ast)
    varColors = assignVarColors allVars
    cVars = collectConstrainedVarNames peeled.constraints
    ctx = { varColors, constrainedVars: cVars }
  in
  "<code class=\"sig-siglet\">"
    <> renderSigletTypeTree ctx peeled.body
  <> "</code>"

-- | Render a full-size signature into a container element.
renderSignatureInto :: String -> { name :: String, ast :: RenderType, typeParams :: Array String, className :: Maybe String } -> Effect Unit
renderSignatureInto selector config = _renderInto selector (renderSignature config)

-- | Render a sigil body (no header) into a container element.
renderBodyInto :: String -> { ast :: RenderType } -> Effect Unit
renderBodyInto selector config = _renderInto selector (renderBody config)

-- | Render a siglet into a container element.
renderSigletInto :: String -> { ast :: RenderType } -> Effect Unit
renderSigletInto selector config = _renderInto selector (renderSiglet config)

-- | Render a signet (dots with rotated identifier labels) to HTML.
-- | Same dot layout as the siglet, but each dot has a 45-degree rotated
-- | label beneath it showing the type identifier name.
renderSignet :: { ast :: RenderType } -> String
renderSignet { ast } =
  let
    peeled = peelSignature ast
    allVars = Set.toUnfoldable (collectTypeVars ast)
    varColors = assignVarColors allVars
    cVars = collectConstrainedVarNames peeled.constraints
    ctx = { varColors, constrainedVars: cVars }
  in
  "<code class=\"sig-signet\">"
    <> renderSignetTypeTree ctx peeled.body
  <> "</code>"

-- | Render a signet into a container element.
renderSignetInto :: String -> { ast :: RenderType } -> Effect Unit
renderSignetInto selector config = _renderInto selector (renderSignet config)

-- | Render a data/newtype declaration to an HTML string.
-- | Shows header (keyword + name + type params), then constructor branches
-- | in a rail layout, or an opaque indicator if no constructors.
renderDataDecl
  :: { name :: String
     , typeParams :: Array String
     , constructors :: Array { name :: String, args :: Array RenderType }
     , keyword :: Maybe String
     }
  -> String
renderDataDecl opts =
  let
    allVars = opts.typeParams <> Array.concatMap (\c -> Array.concatMap (\a -> Set.toUnfoldable (collectTypeVars a)) c.args) opts.constructors
    varColors = assignVarColors (Array.nub allVars)
    ctx = { varColors, constrainedVars: Set.empty }
    kw = case opts.keyword of
      Just k -> k <> " "
      Nothing -> "data "
    isOpaque = Array.null opts.constructors
    outerClass = if isOpaque then "sig-adt sig-adt-opaque" else "sig-adt"
  in
  "<div class=\"" <> outerClass <> "\">"
    <> renderAdtHeader ctx kw opts.name opts.typeParams
    <> (if isOpaque
         then "<div class=\"sig-adt-opaque-label\">no public constructors</div>"
         else renderCtorList ctx opts.constructors)
  <> "</div>"

-- | Render a data declaration into a container element.
renderDataDeclInto
  :: String
  -> { name :: String
     , typeParams :: Array String
     , constructors :: Array { name :: String, args :: Array RenderType }
     , keyword :: Maybe String
     }
  -> Effect Unit
renderDataDeclInto selector config = _renderInto selector (renderDataDecl config)

-- | Render a type class definition to an HTML string.
-- | Shows header (class + name + type params), superclass row,
-- | own methods, and inherited methods grouped by superclass.
renderClassDecl
  :: { name :: String
     , typeParams :: Array String
     , superclasses :: Array SuperclassInfo
     , methods :: Array { name :: String, ast :: Maybe RenderType }
     }
  -> String
renderClassDecl opts =
  let
    varColors = assignVarColors opts.typeParams
    ctx = { varColors, constrainedVars: Set.empty }
    hasSuperclasses = not (Array.null opts.superclasses)
    hasInherited = hasSuperclasses && Array.any (\sc -> not (Array.null sc.methods)) opts.superclasses
  in
  "<div class=\"sig-class\">"
    <> (if hasSuperclasses
         then renderSuperclassRow (map _.name opts.superclasses)
         else "")
    <> renderClassHeader ctx opts.name opts.typeParams
    <> renderMethodList ctx opts.typeParams opts.methods
    <> (if hasInherited
         then renderInheritedSection ctx opts.typeParams opts.superclasses
         else "")
  <> "</div>"

-- | Render a class declaration into a container element.
renderClassDeclInto
  :: String
  -> { name :: String
     , typeParams :: Array String
     , superclasses :: Array SuperclassInfo
     , methods :: Array { name :: String, ast :: Maybe RenderType }
     }
  -> Effect Unit
renderClassDeclInto selector config = _renderInto selector (renderClassDecl config)

-- | Render a type synonym to an HTML string.
-- | Shows header (type + name + type params), then = body.
renderTypeSynonym
  :: { name :: String
     , typeParams :: Array String
     , body :: RenderType
     }
  -> String
renderTypeSynonym opts =
  let
    peeled = peelSignature opts.body
    allVars = opts.typeParams <> peeled.forallVars
              <> (Set.toUnfoldable (collectTypeVars opts.body) :: Array String)
    varColors = assignVarColors (Array.nub allVars)
    ctx = { varColors, constrainedVars: Set.empty }
    hasForall = not (Array.null peeled.forallVars)
    hasConstraints = not (Array.null peeled.constraints)
  in
  "<div class=\"sig-type-alias\">"
    <> "<div class=\"sig-type-alias-header\">"
        <> "<small class=\"sig-kw\">type </small>"
        <> "<dfn class=\"sig-type-alias-name\">" <> escapeHtml opts.name <> "</dfn>"
        <> Array.intercalate "" (opts.typeParams <#> varPill ctx)
      <> "</div>"
    <> "<div class=\"sig-type-alias-body\">"
        <> "<small class=\"sig-type-alias-eq\">=</small>"
        <> (if hasForall
             then "<div class=\"sig-method-forall\">"
                    <> "<small class=\"sig-forall-symbol\">\x2200</small>"
                    <> Array.intercalate "" (peeled.forallVars <#> varPill ctx)
                    <> "<small class=\"sig-forall-dot\">.</small>"
                  <> "</div>"
             else "")
        <> (if hasConstraints
             then "<div class=\"sig-method-constraints\">"
                    <> Array.intercalate "" (peeled.constraints <#> constraintPillTree ctx)
                  <> "</div>"
             else "")
        <> renderTypeTree ctx peeled.body
      <> "</div>"
  <> "</div>"

-- | Render a type synonym into a container element.
renderTypeSynonymInto
  :: String
  -> { name :: String
     , typeParams :: Array String
     , body :: RenderType
     }
  -> Effect Unit
renderTypeSynonymInto selector config = _renderInto selector (renderTypeSynonym config)

-- | Render a foreign import to an HTML string.
-- | Shows header (foreign import + name), then :: body with preamble.
renderForeignImport
  :: { name :: String
     , ast :: RenderType
     }
  -> String
renderForeignImport opts =
  let
    peeled = peelSignature opts.ast
    allVars = Set.toUnfoldable (collectTypeVars opts.ast) <> peeled.forallVars
    varColors = assignVarColors (Array.nub allVars)
    ctx = { varColors, constrainedVars: Set.empty }
    hasPreamble = not (Array.null peeled.forallVars) || not (Array.null peeled.constraints)
  in
  "<div class=\"sig-foreign\">"
    <> "<div class=\"sig-foreign-header\">"
        <> "<small class=\"sig-kw\">foreign </small>"
        <> "<dfn class=\"sig-foreign-name\">" <> escapeHtml opts.name <> "</dfn>"
        <> "<small class=\"sig-dcolon\"> ::</small>"
      <> "</div>"
    <> (if hasPreamble then renderPreamble ctx peeled else "")
    <> "<div class=\"sig-foreign-body\">"
        <> renderTypeTree ctx peeled.body
      <> "</div>"
  <> "</div>"

-- | Render a foreign import into a container element.
renderForeignImportInto
  :: String
  -> { name :: String
     , ast :: RenderType
     }
  -> Effect Unit
renderForeignImportInto selector config = _renderInto selector (renderForeignImport config)

-- =============================================================================
-- Internal types
-- =============================================================================

type TreeCtx =
  { varColors :: Map String String
  , constrainedVars :: Set String
  }

-- | Peeled signature: outermost forall/constraints separated from bare body.
type PeeledSig =
  { forallVars :: Array String
  , constraints :: Array Constraint
  , body :: RenderType
  }

-- | Peel outermost forall quantifiers and constraints from the AST.
-- | Nested foralls/constraints (rank-N, inside parens) are left intact.
peelSignature :: RenderType -> PeeledSig
peelSignature (TForall vars inner) =
  let rest = peelSignature inner
  in rest { forallVars = vars <> rest.forallVars }
peelSignature (TConstrained cs inner) =
  let rest = peelSignature inner
  in rest { constraints = cs <> rest.constraints }
peelSignature body =
  { forallVars: [], constraints: [], body }

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Escape HTML special characters.
escapeHtml :: String -> String
escapeHtml s = go 0 ""
  where
  len = SCU.length s
  go :: Int -> String -> String
  go i acc
    | i >= len = acc
    | otherwise = case SCU.charAt i s of
        Just '<' -> go (i + 1) (acc <> "&lt;")
        Just '>' -> go (i + 1) (acc <> "&gt;")
        Just '&' -> go (i + 1) (acc <> "&amp;")
        Just '"' -> go (i + 1) (acc <> "&quot;")
        Just c   -> go (i + 1) (acc <> SCU.singleton c)
        Nothing  -> acc

-- | Collect all type variable names that appear in constraint arguments.
collectConstrainedVarNames :: Array Constraint -> Set String
collectConstrainedVarNames cs =
  Array.foldl (\acc c ->
    Array.foldl (\acc' arg -> Set.union acc' (collectTypeVars arg)) acc c.args
  ) Set.empty cs

-- | Type variable as a siglet pill (colored background, white text).
-- | Used for constrained vars in siglets.
sigletPill :: TreeCtx -> String -> String
sigletPill ctx name =
  let color = fromMaybe "#0369a1" (Map.lookup name ctx.varColors)
  in "<var class=\"sig-var sig-var-pill\" style=\"--vc:" <> color <> "\">" <> escapeHtml name <> "</var>"

-- =============================================================================
-- Preamble rendering
-- =============================================================================

-- | Render the preamble: quantifier (left) · · · constraints (right).
-- | A dot leader connects quantifier to constraints when both present.
renderPreamble :: TreeCtx -> PeeledSig -> String
renderPreamble ctx peeled =
  let
    hasForall = not (Array.null peeled.forallVars)
    hasConstraints = not (Array.null peeled.constraints)
  in
  "<div class=\"sig-preamble\">"
    <> (if hasForall
         then "<div class=\"sig-quant\">"
                <> "<small class=\"sig-quant-sym\">\x2200</small>"
                <> Array.intercalate "" (peeled.forallVars <#> varPill ctx)
                <> "<small class=\"sig-quant-dot\">.</small>"
              <> "</div>"
         else "")
    <> (if hasForall && hasConstraints
         then "<small class=\"sig-leader\"></small>"
         else "")
    <> (if hasConstraints
         then "<div class=\"sig-ctx\">"
                <> Array.intercalate "" (peeled.constraints <#> renderCtxItem ctx)
              <> "</div>"
         else "")
  <> "</div>"

-- | Render a constraint in the preamble (annotation style, not pill).
renderCtxItem :: TreeCtx -> Constraint -> String
renderCtxItem ctx c =
  "<em class=\"sig-ctx-item\">"
    <> "<code class=\"sig-ctx-class\">" <> escapeHtml c.className <> "</code>"
    <> if Array.null c.args then ""
       else Array.intercalate "" (c.args <#> renderTypeTree ctx)
  <> "</em>"

-- =============================================================================
-- Full-size type tree builder
-- =============================================================================

renderTypeTree :: TreeCtx -> RenderType -> String
renderTypeTree ctx = case _ of
  TVar name ->
    varPill ctx name

  TCon name ->
    if isEffectName name
      then "<code class=\"sig-con sig-con-effect\">" <> escapeHtml name <> "</code>"
      else "<code class=\"sig-con\">" <> escapeHtml name <> "</code>"

  TApp head args ->
    renderAppTree ctx head args

  TArrow from to ->
    renderArrowChainTree ctx (collectArrowParams (TArrow from to))

  TForall vars body ->
    renderForallTree ctx vars body

  TConstrained cs body ->
    "<div class=\"sig-constrained\">"
      <> constraintPile ctx cs
      <> renderTypeTree ctx body
    <> "</div>"

  TParens inner ->
    "<code class=\"sig-app\">"
      <> "<small class=\"sig-paren\">(</small>"
      <> renderTypeTree ctx inner
      <> "<small class=\"sig-paren\">)</small>"
    <> "</code>"

  TRecord fields tail ->
    renderRecordTree ctx fields tail false

  TRow fields tail ->
    renderRecordTree ctx fields tail true

  TOperator l op r ->
    "<code class=\"sig-app\">"
      <> renderTypeTree ctx l
      <> "<small class=\"sig-op\">" <> escapeHtml op <> "</small>"
      <> renderTypeTree ctx r
    <> "</code>"

  TKinded ty kind ->
    "<code class=\"sig-app\">"
      <> renderTypeTree ctx ty
      <> "<small class=\"sig-sep\"> :: </small>"
      <> renderTypeTree ctx kind
    <> "</code>"

  TString s ->
    "<code class=\"sig-string\">" <> escapeHtml ("\"" <> s <> "\"") <> "</code>"

  TWildcard ->
    "<var class=\"sig-var sig-wildcard\">_</var>"

-- | Type variable as a <var> element with color via --vc custom property.
-- | CSS context determines rendering: colored text (default) or pill (siglet).
varPill :: TreeCtx -> String -> String
varPill ctx name =
  let color = fromMaybe "#0369a1" (Map.lookup name ctx.varColors)
  in "<var class=\"sig-var\" style=\"--vc:" <> color <> "\">" <> escapeHtml name <> "</var>"

-- | Type application: head arg1 arg2
-- | Special-cases Record types to drop the keyword and render as table.
renderAppTree :: TreeCtx -> RenderType -> Array RenderType -> String
renderAppTree ctx head args = case head, args of
  -- Record (row) → closed record table, drop "Record" keyword
  TCon "Record", [TRow fields tail] ->
    renderRecordTree ctx fields tail false
  -- Record (+ combination) → row combo table
  TCon "Record", [TParens inner] ->
    let operands = collectPlusOperands inner
    in if Array.length operands > 1
       then renderRowComboTree ctx operands
       else defaultAppTree ctx head args
  -- Default application
  _, _ -> defaultAppTree ctx head args

-- | Default application rendering: HKT gets distinguishing class.
defaultAppTree :: TreeCtx -> RenderType -> Array RenderType -> String
defaultAppTree ctx head args =
  let
    isHkt = case head of
      TVar _ -> true
      _ -> false
    inner = renderTypeTree ctx head <> Array.intercalate "" (args <#> renderAppArgTree ctx)
    cls = if isHkt then "sig-app sig-hkt" else "sig-app"
  in "<code class=\"" <> cls <> "\">" <> inner <> "</code>"

-- | Collect operands from a + operator chain (row combination).
collectPlusOperands :: RenderType -> Array RenderType
collectPlusOperands (TOperator l "+" r) = collectPlusOperands l <> [r]
collectPlusOperands other = [other]

-- | Render row combination as a single-column table of operands.
renderRowComboTree :: TreeCtx -> Array RenderType -> String
renderRowComboTree ctx operands =
  let len = Array.length operands
  in "<ol class=\"sig-row-combo\">"
       <> Array.intercalate "" (Array.mapWithIndex (\i op ->
            let isLast = i == len - 1
            in "<li class=\"sig-combo-item\">"
                 <> renderTypeTree ctx op
                 <> if isLast then ""
                    else "<small class=\"sig-op\"> +</small>"
               <> "</li>"
          ) operands)
     <> "</ol>"

-- | Wrap complex args in parens in application context.
renderAppArgTree :: TreeCtx -> RenderType -> String
renderAppArgTree ctx t = case t of
  TApp _ _ -> wrapParensTree ctx t
  TArrow _ _ -> wrapParensTree ctx t
  TConstrained _ _ -> wrapParensTree ctx t
  TForall _ _ -> wrapParensTree ctx t
  TOperator _ _ _ -> wrapParensTree ctx t
  _ -> renderTypeTree ctx t

wrapParensTree :: TreeCtx -> RenderType -> String
wrapParensTree ctx t =
  "<code class=\"sig-app\">"
    <> "<small class=\"sig-paren\">(</small>"
    <> renderTypeTree ctx t
    <> "<small class=\"sig-paren\">)</small>"
  <> "</code>"

-- | Arrow chain as an ordered list of parameter types.
-- | The → separators are added via CSS (li + li::before).
renderArrowChainTree :: TreeCtx -> Array RenderType -> String
renderArrowChainTree ctx params =
  "<ol class=\"sig-arrow-chain\">"
    <> Array.intercalate "" (params <#> \p ->
         "<li class=\"sig-param\">" <> renderTypeTree ctx p <> "</li>"
       )
  <> "</ol>"

-- | Forall: ∀ vars. body
renderForallTree :: TreeCtx -> Array String -> RenderType -> String
renderForallTree ctx vars body =
  "<div class=\"sig-forall\">"
    <> "<div class=\"sig-forall-row\">"
        <> "<small class=\"sig-forall-symbol\">\x2200</small>"
        <> Array.intercalate "" (vars <#> varPill ctx)
        <> "<small class=\"sig-forall-dot\">.</small>"
      <> "</div>"
    <> renderTypeTree ctx body
  <> "</div>"

-- | Constraint pile: stack of emphasized constraint pills.
constraintPile :: TreeCtx -> Array Constraint -> String
constraintPile ctx cs =
  "<div class=\"sig-constraints\">"
    <> Array.intercalate "" (cs <#> constraintPillTree ctx)
  <> "</div>"

constraintPillTree :: TreeCtx -> Constraint -> String
constraintPillTree ctx c =
  "<em class=\"sig-constraint\">"
    <> "<code class=\"sig-con\">" <> escapeHtml c.className <> "</code>"
    <> if Array.null c.args then ""
       else Array.intercalate "" (c.args <#> renderTypeTree ctx)
  <> "</em>"

-- | Record or row as a 3-column definition list: name | :: | type.
-- | Row tail uses the middle column for | alignment.
renderRecordTree :: TreeCtx -> Array { label :: String, value :: RenderType } -> Maybe String -> Boolean -> String
renderRecordTree ctx fields tail isRow =
  let
    openClass = case tail of
      Just _ -> "sig-record sig-record-open"
      Nothing -> "sig-record"
  in
  if Array.null fields && tail == Nothing then
    "<code class=\"sig-con\">" <> escapeHtml (if isRow then "()" else "{}") <> "</code>"
  else
    "<dl class=\"" <> openClass <> "\">"
      <> Array.intercalate "" (fields <#> \f ->
           "<dt class=\"sig-field-name\">" <> escapeHtml f.label <> "</dt>"
           <> "<dd class=\"sig-field-sep\">::</dd>"
           <> "<dd class=\"sig-field-type\">" <> renderTypeTree ctx f.value <> "</dd>"
         )
      <> case tail of
           Just v ->
             "<dt class=\"sig-field-name\"></dt>"
             <> "<dd class=\"sig-field-sep\">|</dd>"
             <> "<dd class=\"sig-row-tail\">" <> varPill ctx v <> "</dd>"
           Nothing -> ""
    <> "</dl>"

-- =============================================================================
-- Siglet (miniature) type tree builder
-- =============================================================================

-- | Siglet rendering: TCon → dot, multi-letter TVar → dot, otherwise recurse.
-- | Constrained single-letter vars get pill style; unconstrained get colored text.
-- | Records/rows render as `{ ○ ○ ○ }` / `{ ○ ○ ○ |r }`.
renderSigletTypeTree :: TreeCtx -> RenderType -> String
renderSigletTypeTree ctx = case _ of
  TVar name ->
    if String.length name > 1
      then -- Multi-letter var → colored dot
        let color = fromMaybe "#0369a1" (Map.lookup name ctx.varColors)
        in "<var class=\"sig-dot sig-dot-var\" style=\"background:" <> color <> "\"></var>"
      else -- Single-letter var: pill if constrained, colored text if free
        if Set.member name ctx.constrainedVars
          then sigletPill ctx name
          else varPill ctx name

  TCon name ->
    if isEffectName name
      then "<small class=\"sig-dot sig-dot-effect\"></small>"
      else "<small class=\"sig-dot sig-dot-con\"></small>"

  TApp head args ->
    let
      isHkt = case head of
        TVar _ -> true
        _ -> false
      -- Head constructor gets a bigger dot to distinguish applied types
      headHtml = case head of
        TCon name ->
          if isEffectName name
            then "<small class=\"sig-dot sig-dot-effect sig-dot-head\"></small>"
            else "<small class=\"sig-dot sig-dot-con sig-dot-head\"></small>"
        _ -> renderSigletTypeTree ctx head
      argsHtml = Array.intercalate "" (args <#> renderSigletTypeTree ctx)
      cls = if isHkt then "sig-app sig-hkt" else "sig-app"
    in "<code class=\"" <> cls <> "\">" <> headHtml <> argsHtml <> "</code>"

  TArrow from to ->
    let params = collectArrowParams (TArrow from to)
    in "<ol class=\"sig-arrow-chain\">"
         <> Array.intercalate "" (params <#> \p ->
              "<li class=\"sig-param\">" <> renderSigletTypeTree ctx p <> "</li>"
            )
       <> "</ol>"

  TForall _ body ->
    renderSigletTypeTree ctx body

  TConstrained _ body ->
    renderSigletTypeTree ctx body

  TParens inner ->
    "<code class=\"sig-app\">"
      <> "<small class=\"sig-paren\">(</small>"
      <> renderSigletTypeTree ctx inner
      <> "<small class=\"sig-paren\">)</small>"
    <> "</code>"

  TRecord fields tail ->
    renderRecordSiglet ctx "{" "}" fields tail

  TRow fields tail ->
    renderRecordSiglet ctx "(" ")" fields tail

  TOperator l op r ->
    "<code class=\"sig-app\">"
      <> renderSigletTypeTree ctx l
      <> "<small class=\"sig-op\">" <> escapeHtml op <> "</small>"
      <> renderSigletTypeTree ctx r
    <> "</code>"

  TKinded ty _ ->
    renderSigletTypeTree ctx ty

  TString _ ->
    "<code class=\"sig-string\">\"\"</code>"

  TWildcard ->
    "<var class=\"sig-dot sig-dot-var sig-wildcard\"></var>"

-- | Record/row in siglet: `{ ○ ○ ○ }` or `{ ○ ○ ○ |r }`.
-- | One dot per field regardless of field type complexity.
renderRecordSiglet :: TreeCtx -> String -> String -> Array { label :: String, value :: RenderType } -> Maybe String -> String
renderRecordSiglet ctx open close fields tail =
  if Array.null fields && tail == Nothing then
    "<small class=\"sig-dot sig-dot-con\"></small>"
  else
    "<code class=\"sig-record-mini\">"
      <> "<small class=\"sig-brace\">" <> open <> "</small>"
      <> Array.intercalate "" (fields <#> \_ -> "<small class=\"sig-dot sig-dot-con\"></small>")
      <> case tail of
           Just v -> "<small class=\"sig-pipe\">|</small>" <> varPill ctx v
           Nothing -> ""
      <> "<small class=\"sig-brace\">" <> close <> "</small>"
    <> "</code>"

-- =============================================================================
-- Signet (dots + rotated identifier labels) type tree builder
-- =============================================================================

-- | Truncate a label to 15 characters, adding ellipsis if truncated.
truncateLabel :: String -> String
truncateLabel s =
  if String.length s > 15
    then String.take 15 s <> "\x2026"
    else s

-- | Wrap a dot element and its label text in a .sig-lt column container.
labeledToken :: String -> String -> String -> String
labeledToken dotHtml labelText labelColor =
  "<span class=\"sig-lt\">"
    <> dotHtml
    <> "<span class=\"sig-lt-label\" style=\"color:" <> labelColor <> "\">"
        <> escapeHtml (truncateLabel labelText)
      <> "</span>"
  <> "</span>"

-- | Signet rendering: same dots as siglet, plus rotated name labels.
-- | Single-letter free vars render as colored text (already readable, no label).
renderSignetTypeTree :: TreeCtx -> RenderType -> String
renderSignetTypeTree ctx = case _ of
  TVar name ->
    if String.length name > 1
      then -- Multi-letter var → colored dot + label
        let color = fromMaybe "#0369a1" (Map.lookup name ctx.varColors)
        in labeledToken
             ("<var class=\"sig-dot sig-dot-var\" style=\"background:" <> color <> "\"></var>")
             name
             color
      else -- Single-letter var: pill if constrained, colored text if free — no label needed
        if Set.member name ctx.constrainedVars
          then sigletPill ctx name
          else varPill ctx name

  TCon name ->
    if isEffectName name
      then labeledToken
             "<small class=\"sig-dot sig-dot-effect\"></small>"
             name
             "#9333ea"
      else labeledToken
             "<small class=\"sig-dot sig-dot-con\"></small>"
             name
             "#16653e"

  TApp head args ->
    let
      isHkt = case head of
        TVar _ -> true
        _ -> false
      headHtml = case head of
        TCon name ->
          if isEffectName name
            then labeledToken
                   "<small class=\"sig-dot sig-dot-effect sig-dot-head\"></small>"
                   name
                   "#9333ea"
            else labeledToken
                   "<small class=\"sig-dot sig-dot-con sig-dot-head\"></small>"
                   name
                   "#16653e"
        _ -> renderSignetTypeTree ctx head
      argsHtml = Array.intercalate "" (args <#> renderSignetTypeTree ctx)
      cls = if isHkt then "sig-app sig-hkt" else "sig-app"
    in "<code class=\"" <> cls <> "\">" <> headHtml <> argsHtml <> "</code>"

  TArrow from to ->
    let params = collectArrowParams (TArrow from to)
    in "<ol class=\"sig-arrow-chain\">"
         <> Array.intercalate "" (params <#> \p ->
              "<li class=\"sig-param\">" <> renderSignetTypeTree ctx p <> "</li>"
            )
       <> "</ol>"

  TForall _ body ->
    renderSignetTypeTree ctx body

  TConstrained _ body ->
    renderSignetTypeTree ctx body

  TParens inner ->
    "<code class=\"sig-app\">"
      <> "<small class=\"sig-paren\">(</small>"
      <> renderSignetTypeTree ctx inner
      <> "<small class=\"sig-paren\">)</small>"
    <> "</code>"

  TRecord fields tail ->
    renderRecordSignet ctx "{" "}" fields tail

  TRow fields tail ->
    renderRecordSignet ctx "(" ")" fields tail

  TOperator l op r ->
    "<code class=\"sig-app\">"
      <> renderSignetTypeTree ctx l
      <> "<small class=\"sig-op\">" <> escapeHtml op <> "</small>"
      <> renderSignetTypeTree ctx r
    <> "</code>"

  TKinded ty _ ->
    renderSignetTypeTree ctx ty

  TString _ ->
    "<code class=\"sig-string\">\"\"</code>"

  TWildcard ->
    "<var class=\"sig-dot sig-dot-var sig-wildcard\"></var>"

-- | Record/row in signet: `{ ○ ○ ○ }` with field name labels.
renderRecordSignet :: TreeCtx -> String -> String -> Array { label :: String, value :: RenderType } -> Maybe String -> String
renderRecordSignet ctx open close fields tail =
  if Array.null fields && tail == Nothing then
    "<small class=\"sig-dot sig-dot-con\"></small>"
  else
    "<code class=\"sig-record-mini\">"
      <> "<small class=\"sig-brace\">" <> open <> "</small>"
      <> Array.intercalate "" (fields <#> \f ->
           labeledToken
             "<small class=\"sig-dot sig-dot-con\"></small>"
             f.label
             "#16653e"
         )
      <> case tail of
           Just v -> "<small class=\"sig-pipe\">|</small>" <> varPill ctx v
           Nothing -> ""
      <> "<small class=\"sig-brace\">" <> close <> "</small>"
    <> "</code>"

-- =============================================================================
-- ADT rendering helpers
-- =============================================================================

-- | ADT header: keyword + name + type param pills.
renderAdtHeader :: TreeCtx -> String -> String -> Array String -> String
renderAdtHeader ctx kw name typeParams =
  "<div class=\"sig-adt-header\">"
    <> "<small class=\"sig-kw\">" <> escapeHtml kw <> "</small>"
    <> "<dfn class=\"sig-adt-name\">" <> escapeHtml name <> "</dfn>"
    <> Array.intercalate "" (typeParams <#> varPill ctx)
  <> "</div>"

-- | Ordered list of constructor branches.
renderCtorList :: TreeCtx -> Array { name :: String, args :: Array RenderType } -> String
renderCtorList ctx ctors =
  "<ol class=\"sig-adt-ctors\">"
    <> Array.intercalate "" (ctors <#> renderCtorItem ctx)
  <> "</ol>"

-- | Single constructor: name + args.
renderCtorItem :: TreeCtx -> { name :: String, args :: Array RenderType } -> String
renderCtorItem ctx ctor =
  "<li class=\"sig-adt-ctor\">"
    <> "<dfn class=\"sig-adt-ctor-name\">" <> escapeHtml ctor.name <> "</dfn>"
    <> (if Array.null ctor.args
         then "<small class=\"sig-adt-ctor-dash\">\x2014</small>"
         else "<code class=\"sig-adt-ctor-args\">"
                <> Array.intercalate "" (ctor.args <#> renderTypeTree ctx)
              <> "</code>")
  <> "</li>"

-- =============================================================================
-- Class definition rendering helpers
-- =============================================================================

-- | Class header: keyword + name + type param pills.
renderClassHeader :: TreeCtx -> String -> Array String -> String
renderClassHeader ctx name typeParams =
  "<div class=\"sig-class-header\">"
    <> "<small class=\"sig-kw\">class </small>"
    <> "<dfn class=\"sig-class-name\">" <> escapeHtml name <> "</dfn>"
    <> Array.intercalate "" (typeParams <#> varPill ctx)
  <> "</div>"

-- | Superclass row: ⇐ arrow + comma-separated superclass names (above header, right-justified).
renderSuperclassRow :: Array String -> String
renderSuperclassRow names =
  "<div class=\"sig-class-supers\">"
    <> "<small class=\"sig-class-arrow\">&lt;=</small>"
    <> Array.intercalate "<small class=\"sig-class-comma\">, </small>"
        (names <#> \n -> "<em class=\"sig-class-super\">" <> escapeHtml n <> "</em>")
  <> "</div>"

-- | Container for method entries.
renderMethodList :: TreeCtx -> Array String -> Array { name :: String, ast :: Maybe RenderType } -> String
renderMethodList ctx typeParams methods =
  if Array.null methods then
    "<div class=\"sig-class-methods sig-class-no-methods\">"
      <> "<em class=\"sig-class-empty\">(no own methods)</em>"
    <> "</div>"
  else
    "<div class=\"sig-class-methods\">"
      <> Array.intercalate "" (methods <#> renderMethod ctx typeParams)
    <> "</div>"

-- | Single method: peel AST → constraints above, name :: body, forall below.
renderMethod :: TreeCtx -> Array String -> { name :: String, ast :: Maybe RenderType } -> String
renderMethod _ctx0 typeParams m = case m.ast of
  Just ast ->
    let
      peeled = peelSignature ast
      methodVars = typeParams <> collectForallVars ast
                   <> (Set.toUnfoldable (collectTypeVars ast) :: Array String)
      varColors = assignVarColors (Array.nub methodVars)
      ctx = { varColors, constrainedVars: Set.empty }
      hasConstraints = not (Array.null peeled.constraints)
      hasForall = not (Array.null peeled.forallVars)
    in
    "<div class=\"sig-method\">"
      <> (if hasConstraints
           then "<div class=\"sig-method-constraints\">"
                  <> Array.intercalate "" (peeled.constraints <#> constraintPillTree ctx)
                <> "</div>"
           else "")
      <> "<div class=\"sig-method-decl\">"
          <> "<dfn class=\"sig-method-name\">" <> escapeHtml m.name <> "</dfn>"
          <> "<small class=\"sig-method-sep\"> :: </small>"
          <> renderTypeTree ctx peeled.body
        <> "</div>"
      <> (if hasForall
           then "<div class=\"sig-method-forall\">"
                  <> "<small class=\"sig-forall-symbol\">\x2200</small>"
                  <> Array.intercalate "" (peeled.forallVars <#> varPill ctx)
                  <> "<small class=\"sig-forall-dot\">.</small>"
                <> "</div>"
           else "")
    <> "</div>"
  Nothing ->
    "<div class=\"sig-method\">"
      <> "<div class=\"sig-method-decl\">"
          <> "<dfn class=\"sig-method-name\">" <> escapeHtml m.name <> "</dfn>"
        <> "</div>"
    <> "</div>"

-- | Inherited methods section: dashed separator + "via ClassName" groups.
renderInheritedSection :: TreeCtx -> Array String -> Array SuperclassInfo -> String
renderInheritedSection ctx typeParams superclasses =
  let
    groups = Array.concatMap (\sc ->
      if Array.null sc.methods then []
      else
        [ "<div class=\"sig-class-via-group\">"
            <> "<div class=\"sig-class-via\">via " <> escapeHtml sc.name <> "</div>"
            <> Array.intercalate "" (sc.methods <#> renderMethod ctx typeParams)
          <> "</div>"
        ]
    ) superclasses
  in
  if Array.null groups then ""
  else
    "<div class=\"sig-class-inherited\">"
      <> Array.intercalate "" groups
    <> "</div>"

-- | Pure text rendering and type variable collection utilities.
module Sigil.Text
  ( renderTypeToText
  , constraintText
  , collectTypeVars
  , collectArrowParams
  , collectForallVars
  , fieldVars
  ) where

import Prelude
import Prim hiding (Constraint, Row)

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set

import Sigil.Types (RenderType(..), RowField, Constraint)

-- | Render a RenderType back to plain text.
renderTypeToText :: RenderType -> String
renderTypeToText = case _ of
  TVar s -> s
  TCon s -> s
  TApp head args -> renderTypeToText head <> " " <> Array.intercalate " " (map wrapComplex args)
  TArrow from to -> wrapArrow from <> " -> " <> renderTypeToText to
  TConstrained cs body ->
    Array.intercalate ", " (map renderConstraint cs) <> " => " <> renderTypeToText body
  TForall vars body -> "forall " <> Array.intercalate " " vars <> ". " <> renderTypeToText body
  TRecord fields tail -> "{ " <> renderFields fields tail <> " }"
  TRow fields tail -> "( " <> renderFields fields tail <> " )"
  TParens inner -> "(" <> renderTypeToText inner <> ")"
  TKinded ty kind -> renderTypeToText ty <> " :: " <> renderTypeToText kind
  TString s -> "\"" <> s <> "\""
  TWildcard -> "_"
  TOperator l op r -> renderTypeToText l <> " " <> op <> " " <> renderTypeToText r
  where
  wrapComplex :: RenderType -> String
  wrapComplex t = case t of
    TApp _ _ -> "(" <> renderTypeToText t <> ")"
    TArrow _ _ -> "(" <> renderTypeToText t <> ")"
    TConstrained _ _ -> "(" <> renderTypeToText t <> ")"
    TForall _ _ -> "(" <> renderTypeToText t <> ")"
    TOperator _ _ _ -> "(" <> renderTypeToText t <> ")"
    _ -> renderTypeToText t

  wrapArrow :: RenderType -> String
  wrapArrow t = case t of
    TArrow _ _ -> "(" <> renderTypeToText t <> ")"
    _ -> renderTypeToText t

  renderConstraint :: Constraint -> String
  renderConstraint c = c.className <> if Array.null c.args then ""
    else " " <> Array.intercalate " " (map wrapComplex c.args)

  renderFields :: Array RowField -> Maybe String -> String
  renderFields fields tail =
    let fs = Array.intercalate ", " (map (\f -> f.label <> " :: " <> renderTypeToText f.value) fields)
    in case tail of
      Just v -> fs <> " | " <> v
      Nothing -> fs

-- | Render a constraint to plain text (for measurement and display).
constraintText :: Constraint -> String
constraintText c =
  let argsText = map renderTypeToText c.args
  in if Array.null c.args then c.className
     else c.className <> " " <> Array.intercalate " " argsText

-- | Collect all type variable names from a RenderType.
collectTypeVars :: RenderType -> Set String
collectTypeVars = case _ of
  TVar s -> Set.singleton s
  TCon _ -> Set.empty
  TApp head args -> Array.foldl (\acc a -> Set.union acc (collectTypeVars a)) (collectTypeVars head) args
  TArrow from to -> Set.union (collectTypeVars from) (collectTypeVars to)
  TConstrained cs body ->
    let cVars = Array.foldl (\acc c -> Array.foldl (\a2 a -> Set.union a2 (collectTypeVars a)) acc c.args) Set.empty cs
    in Set.union cVars (collectTypeVars body)
  TForall _ body -> collectTypeVars body
  TRecord fields tail -> fieldVars fields tail
  TRow fields tail -> fieldVars fields tail
  TParens inner -> collectTypeVars inner
  TKinded ty _ -> collectTypeVars ty
  TString _ -> Set.empty
  TWildcard -> Set.empty
  TOperator l _ r -> Set.union (collectTypeVars l) (collectTypeVars r)

-- | Collect type variables from record/row fields.
fieldVars :: Array RowField -> Maybe String -> Set String
fieldVars fields tail =
  let fv = Array.foldl (\acc f -> Set.union acc (collectTypeVars f.value)) Set.empty fields
  in case tail of
    Just v -> Set.insert v fv
    Nothing -> fv

-- | Collect all params of an arrow chain (including return type at end).
collectArrowParams :: RenderType -> Array RenderType
collectArrowParams = case _ of
  TArrow from to -> Array.cons from (collectArrowParams to)
  other -> [other]

-- | Collect all forall-quantified variable names (including nested foralls).
collectForallVars :: RenderType -> Array String
collectForallVars = case _ of
  TForall vars body -> vars <> collectForallVars body
  TConstrained _ body -> collectForallVars body
  TArrow from to -> collectForallVars from <> collectForallVars to
  TApp head args -> collectForallVars head <> Array.concatMap collectForallVars args
  TParens inner -> collectForallVars inner
  TRecord fields _ -> Array.concatMap (\f -> collectForallVars f.value) fields
  TRow fields _ -> Array.concatMap (\f -> collectForallVars f.value) fields
  _ -> []

-- | Core type AST for rendering type signatures.
-- |
-- | Language-agnostic representation of type expressions.
module Hylograph.Sigil.Types
  ( RenderType(..)
  , RowField
  , Constraint
  ) where

import Prim hiding (Constraint, Row)
import Data.Maybe (Maybe)

data RenderType
  = TVar String
  | TCon String
  | TApp RenderType (Array RenderType)
  | TArrow RenderType RenderType
  | TConstrained (Array Constraint) RenderType
  | TForall (Array String) RenderType
  | TRecord (Array RowField) (Maybe String)
  | TRow (Array RowField) (Maybe String)
  | TParens RenderType
  | TKinded RenderType RenderType
  | TString String
  | TWildcard
  | TOperator RenderType String RenderType

type RowField = { label :: String, value :: RenderType }
type Constraint = { className :: String, args :: Array RenderType }

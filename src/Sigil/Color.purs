-- | Color palettes and type variable color assignment.
module Sigil.Color
  ( colors
  , adtColors
  , typeVarPalette
  , assignVarColors
  , isEffectName
  , isEffectNameIn
  , defaultEffectNames
  , Colors
  , ADTColors
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))

type Colors =
  { keyword :: String
  , arrow :: String
  , constraint :: String
  , constraintBg :: String
  , constraintBd :: String
  , typevar :: String
  , constructor :: String
  , effect :: String
  , paren :: String
  , separator :: String
  , name :: String
  , fieldName :: String
  , fieldType :: String
  , recordBorder :: String
  , rowVar :: String
  , hktBg :: String
  , hktBd :: String
  , hktText :: String
  , classBg :: String
  , classBd :: String
  , classText :: String
  }

colors :: Colors
colors =
  { keyword:      "#7b61ff"
  , arrow:        "#71717a"
  , constraint:   "#92400e"
  , constraintBg: "#fef3c7"
  , constraintBd: "#d97706"
  , typevar:      "#0369a1"
  , constructor:  "#16653e"
  , effect:       "#9333ea"
  , paren:        "#aaa"
  , separator:    "#aaa"
  , name:         "#222"
  , fieldName:    "#333"
  , fieldType:    "#555"
  , recordBorder: "#ccc"
  , rowVar:       "#0369a1"
  , hktBg:        "#eff6ff"
  , hktBd:        "#3b82f6"
  , hktText:      "#1d4ed8"
  , classBg:      "#f0f4ff"
  , classBd:      "#6366f1"
  , classText:    "#4338ca"
  }

type ADTColors =
  { headerBg :: String
  , headerBd :: String
  , headerText :: String
  , rail :: String
  , ctorName :: String
  , ctorBranch :: String
  }

adtColors :: ADTColors
adtColors =
  { headerBg:   "#fef3c7"
  , headerBd:   "#d97706"
  , headerText: "#92400e"
  , rail:       "#d97706"
  , ctorName:   "#b45309"
  , ctorBranch: "#d97706"
  }

-- | Tailwind 600-level palette for type variable pills (white text on colored bg).
typeVarPalette :: Array String
typeVarPalette =
  [ "#0284c7" -- sky
  , "#d97706" -- amber
  , "#059669" -- emerald
  , "#7c3aed" -- violet
  , "#dc2626" -- red
  , "#0d9488" -- teal
  , "#c026d3" -- fuchsia
  , "#4f46e5" -- indigo
  ]

-- | Assign colors to type variables. Earlier variables get earlier palette slots.
assignVarColors :: Array String -> Map String String
assignVarColors vars =
  let
    unique = Array.nub vars
    paletteLen = Array.length typeVarPalette
    pairs = Array.mapWithIndex (\i v ->
      Tuple v (fromMaybe "#0369a1" (Array.index typeVarPalette (i `mod` paletteLen)))
    ) unique
  in Map.fromFoldable pairs

defaultEffectNames :: Set.Set String
defaultEffectNames = Set.fromFoldable ["Effect", "Aff", "MonadAff", "MonadEffect"]

isEffectName :: String -> Boolean
isEffectName = isEffectNameIn defaultEffectNames

isEffectNameIn :: Set.Set String -> String -> Boolean
isEffectNameIn names name = Set.member name names

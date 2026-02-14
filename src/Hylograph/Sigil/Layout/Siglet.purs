-- | Siglet layout: elided type signature showing arity, structure, and variable patterns.
-- |
-- | Concrete type names become colored circles; type variable pills keep their colors.
-- | Forall and constraint annotations are preserved.
module Hylograph.Sigil.Layout.Siglet
  ( layoutSiglet
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set as Set

import Hylograph.Sigil.Types (RenderType)
import Hylograph.Sigil.Types.Layout (LayoutNode(..))
import Hylograph.Sigil.Color (assignVarColors)
import Hylograph.Sigil.Text (collectForallVars, collectTypeVars)
import Hylograph.Sigil.Measure (defaultRenderContext, measure)
import Hylograph.Sigil.Layout (renderNode)

-- | Lay out a siglet (elided miniature type signature).
-- | TCon names render as small circles; TVar pills keep colors.
-- | Returns Nothing if the type can't fit.
layoutSiglet
  :: { ast :: RenderType
     , maxWidth :: Number
     , maxHeight :: Number
     }
  -> Maybe { layout :: LayoutNode, scaledWidth :: Number, scaledHeight :: Number }
layoutSiglet opts =
  let
    allVars = collectForallVars opts.ast
               <> (Set.toUnfoldable (collectTypeVars opts.ast) :: Array String)
    varColors = assignVarColors allVars
    ctx = defaultRenderContext { varColors = varColors, sigletMode = true }

    dims = measure ctx opts.ast
    fullW = dims.width + 10.0
    fullH = max dims.height ctx.lineHeight + 4.0
  in
    if fullW <= 0.0 || fullH <= 0.0 then Nothing
    else
      let
        scaleX = opts.maxWidth / fullW
        scaleY = opts.maxHeight / fullH
        scale = min (min scaleX scaleY) 1.0

        bodyR = renderNode ctx 0.0 0.0 opts.ast

        inner = LGroup
          { transform: "scale(" <> show scale <> ")"
          , children: bodyR.nodes
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

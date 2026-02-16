# Sigil Library — Deficiencies Register

Reviewed 2026-02-14. To be revisited after SVG→HTML migration.

## Architecture

### A1. Measure/Layout Drift
**Status:** Open
**Severity:** High

`measure` and `renderNode` both pattern-match the full `RenderType` AST independently. Magic numbers duplicated (e.g. `24.0` per constraint row, `28.0` header height, `10.0` constraint gap). Changes to one traversal can silently break the other.

**Fix:** Eliminate `Measure.purs` entirely. With HTML output, the browser is the layout engine. For any remaining SVG path (freeform graphics), derive measurement from a single traversal rather than maintaining two.

### A2. sigletMode Boolean Flag
**Status:** Open
**Severity:** Medium

`RenderContext.sigletMode :: Boolean` scatters `if sigletMode then ... else ...` through Measure, Layout, and the constraint renderer. Doesn't scale to additional modes (arity polygon, HTML output, etc.).

**Fix:** Replace with a render mode ADT or strategy record:
```purescript
data RenderMode = Full | Siglet | ArityGlyph

-- or: record of per-node handlers
type NodeStrategy =
  { renderTCon :: Context -> String -> output
  , renderTVar :: Context -> String -> output
  , ...
  }
```

### A3. Annotation Zone Duplication
**Status:** Open
**Severity:** Medium

The "constraint pile above, body middle, forall row below" pattern is implemented independently in Signature.purs, Siglet.purs, and ClassDef.purs (per-method). Each calculates constraint height the same way, renders forall rows similarly but slightly differently, and manages Y offsets manually.

**Fix:** Extract a shared `layoutAnnotated` combinator that handles the three-zone pattern once, parameterised by body content.

### A4. No Intermediate Representation for HTML Path
**Status:** Open (new, from HTML migration decision)
**Severity:** Medium

Currently: `RenderType → LayoutNode → SVG DOM`. For HTML output the pipeline should be `RenderType → Halogen HTML` directly, with `LayoutNode` retained only for freeform SVG graphics (ADT rails, polygons). The two paths need a clean split point.

**Fix:** Create `Sigil.Html` module (RenderType → HH.HTML) as the primary output path. Retain `Sigil.Svg` for freeform graphics only.

## Code Quality

### C1. Index-Based Folds
**Status:** Open
**Severity:** Low

`Array.foldl` over `Array.range 0 (n-1)` with `Array.index` appears ~15 times. This is imperative loop-by-index, not idiomatic PureScript.

**Fix:** Replace with `Array.mapWithIndex`, direct `Array.foldl` over array elements, or `mapAccumL` where accumulator + transform are both needed.

### C2. Inline CSS Strings
**Status:** Open
**Severity:** Medium

Every renderer constructs CSS strings by concatenation:
```purescript
, style: "fill:" <> colors.constraintBg <> ";stroke:" <> colors.constraintBd <> ";stroke-width:1;"
```

No theming, no dark mode, no consumer customization. Typo-prone.

**Fix:** With HTML output, use CSS classes and CSS custom properties instead. The `Colors` record maps to `--sigil-keyword`, `--sigil-arrow`, etc. Renderers emit class names, the stylesheet provides values.

### C3. No Typeclass Instances on Core Types
**Status:** Open
**Severity:** Low

`RenderType` and `LayoutNode` have no `Eq`, `Show`, or generic fold/traversal. Makes testing and debugging hard.

**Fix:** Derive `Eq`, `Show` (or `Generic` + `genericShow`). Consider a `foldRenderType` catamorphism for generic traversals (collectTypeVars, renderTypeToText, etc. are all manual recursions over the same structure).

### C4. No Tests
**Status:** Open
**Severity:** Medium

Published library with zero test coverage. Text.purs, Color.purs, and Measure.purs are all pure functions that are trivially testable.

**Fix:** Add `spec-discover` test suite. Priority targets: `renderTypeToText` round-trips, `collectTypeVars` completeness, `assignVarColors` determinism, `measure` consistency.

## Domain Coupling

### D1. Hardcoded PureScript Knowledge in Library
**Status:** Open
**Severity:** Medium

`effectNames = Set.fromFoldable ["Effect", "Aff", "MonadAff", "MonadEffect"]` in Color.purs. The library shouldn't know which types are "effects" — that's consumer-domain knowledge.

**Fix:** Move `effectNames` / `isEffectName` to a configuration field on `RenderContext` (or the theme/strategy). Consumer provides the set of names that get special treatment.

### D2. RenderType is PureScript-Shaped
**Status:** Acknowledged, deferred
**Severity:** Low (for current use case)

`TRecord`, `TRow`, `TKinded`, `SuperclassInfo` are PureScript/Haskell-specific. Not a problem today (CodeExplorer is PureScript-only) but noted for future reference.

**Fix (deferred):** If multi-language support becomes needed, split into core `TypeExpr` (TVar, TCon, TApp, TArrow, TParens, TWildcard, TString) + language-specific extensions.

## FFI

### F1. SVG DOM FFI (Emit.js)
**Status:** Open
**Severity:** Medium

Four FFI functions (`createSvgElement`, `setAttr`, `appendChild`, `setTextContent`) used to build SVG DOM. Thin and correct, but FFI is FFI.

**Fix:** With HTML output, the primary path uses Halogen's native HTML generation — zero FFI. If an SVG path is retained for freeform graphics, consider using `purescript-web-dom` / `purescript-web-html` bindings instead of custom FFI, or keep the minimal FFI for the SVG-only path and document it clearly.

### F2. TypeSignature.js Consumer FFI
**Status:** Open
**Severity:** Medium

The consumer (minard frontend) has its own FFI in `TypeSignature.js`: `replaceContainerContent`, `showFallbackText`, `insertSVGIntoCell`, `showSigletTooltip`, `hideSigletTooltip`, plus sparkline DOM helpers. These exist because SVG elements can't be produced by Halogen.

**Fix:** HTML sigils render natively in Halogen — no `insertSVGIntoCell`, no `replaceContainerContent`. Tooltip can be a Halogen child component or CSS-only `:hover` with `position: fixed`. Eliminates ~100 lines of JS FFI in the consumer.

## LayoutNode Limitations

### L1. No Polygon/Path Primitive
**Status:** Open
**Severity:** Low (until arity glyph is needed)

`LayoutNode` supports Text, Rect, Line, Circle, Group. No polygon, path, or arc. The arity-polygon visualisation would need `LPolygon` or `LPath`.

**Fix:** Add when needed. Emit.purs update is ~10 lines per new primitive. Only relevant for the SVG freeform path, not the HTML path.

---

## Post-Migration Checklist

After SVG→HTML migration, revisit each item and mark:
- [x] Fixed by migration
- [ ] Still needs work
- [ ] No longer relevant

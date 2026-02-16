-- | Test suite for sigil.
-- |
-- | Tests all 50 curated examples from the shared Examples module, plus
-- | unit tests for parse roundtrip, HTML structure, color assignment, peel,
-- | escapeHtml, and elide.
module Test.Main where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.String as String
import Effect (Effect)
import Effect.Console (log)

import Sigil.Types (RenderType(..))
import Sigil.Text (renderTypeToText)
import Sigil.Color (assignVarColors)
import Sigil.Parse (parseToRenderType, elideAST)
import Sigil.Html (renderSignature, renderBody, renderSiglet, renderDataDecl, renderClassDecl, renderTypeSynonym, renderForeignImport, peelSignature, escapeHtml)
import Sigil.Examples (signatures, dataDecls, classDecls, typeSynonyms, foreignImports)

main :: Effect Unit
main = do
  log "=== sigil tests ==="
  log ""
  testAllExamplesParse
  testAllExamplesRender
  testAllExamplesSiglet
  testParseRoundtrip
  testHtmlOutput
  testColorAssignment
  testPeel
  testEscapeHtml
  testElide
  testDataDeclRender
  testClassDeclRender
  testTypeSynonymRender
  testForeignImportRender
  log ""
  log "=== All tests passed ==="

-- ============================================================
-- All examples: parse
-- ============================================================

testAllExamplesParse :: Effect Unit
testAllExamplesParse = do
  log "-- All examples parse --"
  let total = Array.length signatures
  failed <- Array.foldM (\acc s ->
    case parseToRenderType s.sig of
      Just _ -> pure acc
      Nothing -> do
        log $ "  FAIL #" <> show s.idx <> " " <> s.name <> ": parse failed"
        pure (acc + 1)
  ) 0 signatures
  when (failed > 0) do
    log $ "  " <> show failed <> "/" <> show total <> " examples failed to parse"
    assert false
  log $ "  OK (" <> show total <> "/" <> show total <> " parsed)"

-- ============================================================
-- All examples: full signature render (no crash, produces HTML)
-- ============================================================

testAllExamplesRender :: Effect Unit
testAllExamplesRender = do
  log "-- All examples render signature --"
  let total = Array.length signatures
  failed <- Array.foldM (\acc s ->
    case parseToRenderType s.sig of
      Just ast -> do
        let html = renderSignature { name: s.name, ast, typeParams: [], className: Nothing }
        if String.contains (String.Pattern "sig-full") html
          then pure acc
          else do
            log $ "  FAIL #" <> show s.idx <> " " <> s.name <> ": missing sig-full in output"
            pure (acc + 1)
      Nothing -> pure acc  -- parse failures caught by testAllExamplesParse
  ) 0 signatures
  when (failed > 0) do
    log $ "  " <> show failed <> "/" <> show total <> " render failures"
    assert false
  log $ "  OK (" <> show total <> " rendered)"

-- ============================================================
-- All examples: siglet render (no crash, produces HTML)
-- ============================================================

testAllExamplesSiglet :: Effect Unit
testAllExamplesSiglet = do
  log "-- All examples render siglet --"
  let total = Array.length signatures
  failed <- Array.foldM (\acc s ->
    case parseToRenderType s.sig of
      Just ast -> do
        let html = renderSiglet { ast: elideAST ast }
        if String.contains (String.Pattern "sig-siglet") html
          then pure acc
          else do
            log $ "  FAIL #" <> show s.idx <> " " <> s.name <> ": missing sig-siglet in output"
            pure (acc + 1)
      Nothing -> pure acc
  ) 0 signatures
  when (failed > 0) do
    log $ "  " <> show failed <> "/" <> show total <> " siglet failures"
    assert false
  log $ "  OK (" <> show total <> " rendered)"

-- ============================================================
-- Parse roundtrip tests
-- ============================================================

testParseRoundtrip :: Effect Unit
testParseRoundtrip = do
  log "-- Parse roundtrip --"

  assertParseRoundtrip "Int" "Int"
  assertParseRoundtrip "forall a. a -> a" "forall a. a -> a"
  assertParseRoundtrip "forall a. Ord a => Array a -> Array a"
                       "forall a. Ord a => Array a -> Array a"
  assertParseRoundtrip "Record ( x :: Number, y :: Number )"
                       "Record ( x :: Number, y :: Number )"
  assertParseRoundtrip "forall a. (forall s. ST s a) -> a"
                       "forall a. (forall s. ST s a) -> a"
  assertParseRoundtrip "( id :: NodeID | row )"
                       "( id :: NodeID | row )"

  -- Sanitization: ", |" → " |"
  assertParseable "( x :: Int, | r )"

  log "  OK"

assertParseRoundtrip :: String -> String -> Effect Unit
assertParseRoundtrip input expected = case parseToRenderType input of
  Nothing -> do
    log $ "  FAIL: could not parse: " <> input
    assert false
  Just ast ->
    let result = renderTypeToText ast
    in when (result /= expected) do
      log $ "  FAIL: roundtrip mismatch"
      log $ "    input:    " <> input
      log $ "    expected: " <> expected
      log $ "    got:      " <> result
      assert false

assertParseable :: String -> Effect Unit
assertParseable input = when (not (isJust (parseToRenderType input))) do
  log $ "  FAIL: expected parseable: " <> input
  assert false

-- ============================================================
-- HTML output tests
-- ============================================================

testHtmlOutput :: Effect Unit
testHtmlOutput = do
  log "-- HTML output --"

  -- Simple value: produces sig-full with sig-con
  let simpleHtml = renderSignature { name: "x", ast: TCon "Int", typeParams: [], className: Nothing }
  assertContains simpleHtml "sig-full" "sig-full class"
  assertContains simpleHtml "sig-name" "sig-name for declaration"
  assertContains simpleHtml "sig-con" "sig-con for constructor"
  assertContains simpleHtml ">Int<" "Int text"

  -- Arrow chain: produces sig-arrow-chain with <li> elements
  case parseToRenderType "Int -> String -> Boolean" of
    Just ast -> do
      let arrowHtml = renderSignature { name: "f", ast, typeParams: [], className: Nothing }
      assertContains arrowHtml "sig-arrow-chain" "arrow chain"
      assertContains arrowHtml "<li" "list items for arrow params"
    Nothing -> assert false

  -- Constrained: produces sig-ctx for preamble constraints
  case parseToRenderType "forall a. Ord a => Array a -> Array a" of
    Just ast -> do
      let constrainedHtml = renderSignature { name: "sort", ast, typeParams: [], className: Nothing }
      assertContains constrainedHtml "sig-preamble" "preamble div"
      assertContains constrainedHtml "sig-ctx" "constraint context"
      assertContains constrainedHtml "sig-quant" "quantifier"
      assertContains constrainedHtml "sig-var" "type variable"
    Nothing -> assert false

  -- Record: produces sig-record with <dl>
  case parseToRenderType "Record ( x :: Number, y :: Number )" of
    Just ast -> do
      let recordHtml = renderBody { ast }
      assertContains recordHtml "sig-record" "record class"
      assertContains recordHtml "<dl" "definition list"
      assertContains recordHtml "<dt" "field name dt"
    Nothing -> assert false

  -- Siglet: produces sig-siglet with sig-dot
  case parseToRenderType "Int -> String" of
    Just ast -> do
      let sigletHtml = renderSiglet { ast: elideAST ast }
      assertContains sigletHtml "sig-siglet" "siglet class"
      assertContains sigletHtml "sig-dot" "dot indicator"
    Nothing -> assert false

  -- Siglet constrained: constrained vars get pill, unconstrained get text
  case parseToRenderType "forall a b. Ord a => a -> b -> Array a" of
    Just ast -> do
      let sigletHtml = renderSiglet { ast: elideAST ast }
      assertContains sigletHtml "sig-var-pill" "constrained var pill"
      -- 'b' is unconstrained, should NOT have pill class
      -- (it will have sig-var but not sig-var-pill)
    Nothing -> assert false

  -- Siglet record: produces sig-record-mini with braces
  case parseToRenderType "Record ( x :: Int, y :: Int )" of
    Just ast -> do
      let sigletHtml = renderSiglet { ast: elideAST ast }
      assertContains sigletHtml "sig-record-mini" "record mini"
      assertContains sigletHtml "sig-brace" "brace"
    Nothing -> assert false

  -- Siglet TApp head: produces sig-dot-head for applied constructor
  case parseToRenderType "Maybe a" of
    Just ast -> do
      let sigletHtml = renderSiglet { ast: elideAST ast }
      assertContains sigletHtml "sig-dot-head" "head constructor big dot"
    Nothing -> assert false

  -- Rank-N: produces nested sig-forall
  case parseToRenderType "forall a. (forall s. ST s a) -> a" of
    Just ast -> do
      let rankNHtml = renderSignature { name: "runST", ast, typeParams: [], className: Nothing }
      assertContains rankNHtml "sig-forall" "nested forall"
    Nothing -> assert false

  log "  OK"

assertContains :: String -> String -> String -> Effect Unit
assertContains haystack needle label =
  when (not (String.contains (String.Pattern needle) haystack)) do
    log $ "  FAIL: expected '" <> needle <> "' in output (" <> label <> ")"
    log $ "    output: " <> String.take 200 haystack
    assert false

-- ============================================================
-- Color assignment tests
-- ============================================================

testColorAssignment :: Effect Unit
testColorAssignment = do
  log "-- Color assignment --"

  let colors = assignVarColors ["a", "b", "c"]
  when (Map.size colors /= 3) do
    log $ "  FAIL: expected 3 colors, got " <> show (Map.size colors)
    assert false

  when (not (isJust (Map.lookup "a" colors))) do
    log "  FAIL: no color for 'a'"
    assert false
  when (not (isJust (Map.lookup "b" colors))) do
    log "  FAIL: no color for 'b'"
    assert false

  case Map.lookup "a" colors, Map.lookup "b" colors of
    Just ca, Just cb -> when (ca == cb) do
      log "  FAIL: 'a' and 'b' got same color"
      assert false
    _, _ -> assert false

  let colors2 = assignVarColors ["a", "b", "a", "c"]
  when (Map.size colors2 /= 3) do
    log $ "  FAIL: expected 3 unique colors, got " <> show (Map.size colors2)
    assert false

  log "  OK"

-- ============================================================
-- Peel tests
-- ============================================================

testPeel :: Effect Unit
testPeel = do
  log "-- Peel --"

  let p1 = peelSignature (TCon "Int")
  when (not (Array.null p1.forallVars)) do
    log "  FAIL: expected no forallVars for TCon"
    assert false
  when (not (Array.null p1.constraints)) do
    log "  FAIL: expected no constraints for TCon"
    assert false

  let p2 = peelSignature (TForall ["a"] (TVar "a"))
  when (p2.forallVars /= ["a"]) do
    log $ "  FAIL: expected forallVars [a], got " <> show p2.forallVars
    assert false

  case parseToRenderType "forall a. Ord a => Array a -> Array a" of
    Just ast -> do
      let p3 = peelSignature ast
      when (p3.forallVars /= ["a"]) do
        log $ "  FAIL: expected forallVars [a], got " <> show p3.forallVars
        assert false
      when (Array.length p3.constraints /= 1) do
        log $ "  FAIL: expected 1 constraint, got " <> show (Array.length p3.constraints)
        assert false
    Nothing -> assert false

  -- Multi-constraint peel
  case parseToRenderType "forall f m a b. Foldable f => Monad m => m b -> f a -> b" of
    Just ast -> do
      let p4 = peelSignature ast
      when (Array.length p4.forallVars /= 4) do
        log $ "  FAIL: expected 4 forallVars, got " <> show (Array.length p4.forallVars)
        assert false
      when (Array.length p4.constraints /= 2) do
        log $ "  FAIL: expected 2 constraints, got " <> show (Array.length p4.constraints)
        assert false
    Nothing -> assert false

  log "  OK"

-- ============================================================
-- Escape HTML tests
-- ============================================================

testEscapeHtml :: Effect Unit
testEscapeHtml = do
  log "-- Escape HTML --"

  assert (escapeHtml "hello" == "hello")
  assert (escapeHtml "<script>" == "&lt;script&gt;")
  assert (escapeHtml "a & b" == "a &amp; b")
  assert (escapeHtml "\"quoted\"" == "&quot;quoted&quot;")

  log "  OK"

-- ============================================================
-- Elide tests
-- ============================================================

testElide :: Effect Unit
testElide = do
  log "-- Elide --"

  -- Record preserves structure
  case elideAST (TRecord [{ label: "x", value: TCon "Int" }] Nothing) of
    TRecord [{ label: "x", value: TCon "Int" }] Nothing -> pure unit
    _ -> do
      log "  FAIL: expected TRecord to preserve structure"
      assert false

  -- Row preserves structure with tail
  case elideAST (TRow [{ label: "x", value: TCon "Int" }] (Just "r")) of
    TRow [{ label: "x", value: TCon "Int" }] (Just "r") -> pure unit
    _ -> do
      log "  FAIL: expected TRow to preserve structure"
      assert false

  -- Record(row) normalizes to TRecord
  case elideAST (TApp (TCon "Record") [TRow [{ label: "x", value: TCon "Int" }] Nothing]) of
    TRecord _ Nothing -> pure unit
    _ -> do
      log "  FAIL: expected Record(row) to normalize to TRecord"
      assert false

  -- Arrow preserved
  case elideAST (TArrow (TCon "Int") (TCon "String")) of
    TArrow (TCon "Int") (TCon "String") -> pure unit
    _ -> do
      log "  FAIL: expected arrow to be preserved"
      assert false

  -- Constraints preserved
  case parseToRenderType "forall a. Ord a => Array a -> a" of
    Just ast -> case elideAST ast of
      TForall _ (TConstrained cs _) -> when (Array.length cs /= 1) do
        log "  FAIL: expected constraint preserved in elideAST"
        assert false
      _ -> do
        log "  FAIL: expected TForall + TConstrained after elide"
        assert false
    Nothing -> assert false

  -- Record args in TApp collapse (e.g. Step Record (row) d)
  let stepAst = TApp (TCon "Step") [TCon "Record", TRow [{ label: "x", value: TCon "Int" }] Nothing, TVar "d"]
  case elideAST stepAst of
    TApp (TCon "Step") args -> case Array.index args 0 of
      Just (TRecord _ _) -> when (Array.length args /= 2) do
        log $ "  FAIL: expected 2 args after collapse, got " <> show (Array.length args)
        assert false
      _ -> do
        log "  FAIL: expected first arg to be TRecord after collapse"
        assert false
    _ -> do
      log "  FAIL: expected TApp after elide"
      assert false

  log "  OK"

-- ============================================================
-- Data declaration render tests
-- ============================================================

testDataDeclRender :: Effect Unit
testDataDeclRender = do
  log "-- Data declaration render --"
  let total = Array.length dataDecls
  failed <- Array.foldM (\acc d ->
    let
      ctors = map (\c ->
        { name: c.name
        , args: Array.concatMap (\s -> case parseToRenderType s of
            Just rt -> [rt]
            Nothing -> []
          ) c.argSigs
        }
      ) d.constructors
      html = renderDataDecl
        { name: d.name, typeParams: d.typeParams, constructors: ctors, keyword: d.keyword }
    in do
      -- All should contain sig-adt
      if not (String.contains (String.Pattern "sig-adt") html) then do
        log $ "  FAIL #" <> show d.idx <> " " <> d.name <> ": missing sig-adt"
        pure (acc + 1)
      -- Opaque type should have opaque label
      else if Array.null d.constructors then
        if not (String.contains (String.Pattern "sig-adt-opaque-label") html) then do
          log $ "  FAIL #" <> show d.idx <> " " <> d.name <> ": missing opaque label"
          pure (acc + 1)
        else pure acc
      -- Non-opaque should have constructor names
      else do
        let ctorFailed = Array.foldl (\cf c ->
              if not (String.contains (String.Pattern c.name) html) then cf + 1 else cf
            ) 0 d.constructors
        when (ctorFailed > 0) do
          log $ "  FAIL #" <> show d.idx <> " " <> d.name <> ": missing constructor names"
        pure (acc + ctorFailed)
  ) 0 dataDecls
  when (failed > 0) do
    log $ "  " <> show failed <> " data decl test failures"
    assert false
  log $ "  OK (" <> show total <> " data decls rendered)"

-- ============================================================
-- Class declaration render tests
-- ============================================================

testClassDeclRender :: Effect Unit
testClassDeclRender = do
  log "-- Class declaration render --"
  let total = Array.length classDecls
  failed <- Array.foldM (\acc c ->
    let
      supers = map (\sc ->
        { name: sc.name
        , methods: map (\m ->
            { name: m.name, ast: m.sig >>= parseToRenderType }
          ) sc.methods
        }
      ) c.superclasses
      methods = map (\m ->
        { name: m.name, ast: m.sig >>= parseToRenderType }
      ) c.methods
      html = renderClassDecl
        { name: c.name, typeParams: c.typeParams, superclasses: supers, methods }
    in do
      -- All should contain sig-class
      if not (String.contains (String.Pattern "sig-class") html) then do
        log $ "  FAIL #" <> show c.idx <> " " <> c.name <> ": missing sig-class"
        pure (acc + 1)
      -- Check method names present
      else do
        let methodFailed = Array.foldl (\mf m ->
              if not (String.contains (String.Pattern m.name) html) then mf + 1 else mf
            ) 0 c.methods
        -- Check superclass names present
        let superFailed = Array.foldl (\sf sc ->
              if not (String.contains (String.Pattern sc.name) html) then sf + 1 else sf
            ) 0 c.superclasses
        -- Check "via" present when superclass has methods
        let hasInherited = Array.any (\sc -> not (Array.null sc.methods)) c.superclasses
        let viaFailed = if hasInherited && not (String.contains (String.Pattern "sig-class-via") html)
              then 1 else 0
        let totalFailed = methodFailed + superFailed + viaFailed
        when (totalFailed > 0) do
          log $ "  FAIL #" <> show c.idx <> " " <> c.name <> ": " <> show totalFailed <> " checks failed"
        pure (acc + totalFailed)
  ) 0 classDecls
  when (failed > 0) do
    log $ "  " <> show failed <> " class decl test failures"
    assert false
  log $ "  OK (" <> show total <> " class decls rendered)"

-- ============================================================
-- Type synonym render tests
-- ============================================================

testTypeSynonymRender :: Effect Unit
testTypeSynonymRender = do
  log "-- Type synonym render --"
  let total = Array.length typeSynonyms
  failed <- Array.foldM (\acc t ->
    case parseToRenderType t.body of
      Just body ->
        let html = renderTypeSynonym { name: t.name, typeParams: t.typeParams, body }
        in if not (String.contains (String.Pattern "sig-type-alias") html) then do
          log $ "  FAIL #" <> show t.idx <> " " <> t.name <> ": missing sig-type-alias"
          pure (acc + 1)
        else if not (String.contains (String.Pattern t.name) html) then do
          log $ "  FAIL #" <> show t.idx <> " " <> t.name <> ": missing name"
          pure (acc + 1)
        else pure acc
      Nothing -> do
        log $ "  FAIL #" <> show t.idx <> " " <> t.name <> ": parse failed"
        pure (acc + 1)
  ) 0 typeSynonyms
  when (failed > 0) do
    log $ "  " <> show failed <> " type synonym test failures"
    assert false
  log $ "  OK (" <> show total <> " type synonyms rendered)"

-- ============================================================
-- Foreign import render tests
-- ============================================================

testForeignImportRender :: Effect Unit
testForeignImportRender = do
  log "-- Foreign import render --"
  let total = Array.length foreignImports
  failed <- Array.foldM (\acc f ->
    case parseToRenderType f.sig of
      Just ast ->
        let html = renderForeignImport { name: f.name, ast }
        in if not (String.contains (String.Pattern "sig-foreign") html) then do
          log $ "  FAIL #" <> show f.idx <> " " <> f.name <> ": missing sig-foreign"
          pure (acc + 1)
        else if not (String.contains (String.Pattern f.name) html) then do
          log $ "  FAIL #" <> show f.idx <> " " <> f.name <> ": missing name"
          pure (acc + 1)
        else if not (String.contains (String.Pattern "foreign") html) then do
          log $ "  FAIL #" <> show f.idx <> " " <> f.name <> ": missing keyword"
          pure (acc + 1)
        else pure acc
      Nothing -> do
        log $ "  FAIL #" <> show f.idx <> " " <> f.name <> ": parse failed"
        pure (acc + 1)
  ) 0 foreignImports
  when (failed > 0) do
    log $ "  " <> show failed <> " foreign import test failures"
    assert false
  log $ "  OK (" <> show total <> " foreign imports rendered)"

-- ============================================================
-- Helpers
-- ============================================================

assert :: Boolean -> Effect Unit
assert true = pure unit
assert false = throwError "Assertion failed"

foreign import throwError :: forall a. String -> Effect a

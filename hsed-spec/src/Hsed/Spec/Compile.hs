{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Hsed.Spec.Compile
Description : The bound tables compiled to Gen's declarations, and the generated module
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

@hsedc compile@ (design section 5.2): the checks of "Hsed.Spec.Check", then one
declaration per bound table of the two M1 shapes (the design's per-shape
@Compile'@ class, simplified to one function per shape):

* a type row gives a 'Gen.TypeRowDecl': the UID bytes from its cell, the Format
  text and the 'TypeDef' 'Hsed.Spec.Format.parseFormat' makes of it under the
  errata set, and its provenance (1-based PDF page, printed page = page -
  labelOffset, structure ordinal and path, sha256 of the cell texts the way
  @tools/extract/annotate.py@ hashes them for the manifest);
* an enumeration gives a 'Gen.EnumDecl' with the range and UID of the type row
  it joins.

A bound table that cannot be generated cleanly is left out with the Warning
@W-GEN-EXCLUDED@ saying why (its Error is already among the findings). The
module text is 'Gen.renderModule''s: deterministic, sorted by table number.
-}
module Hsed.Spec.Compile
  ( CompileResult (..)
  , compileSpec
  , compileIR
  , typeRowDecl
  , enumDecl
  , provenanceOf
  , cellsHash
  , generatedModuleName
  , generatedModulePath
  ) where

import           Control.Monad.RWS.Strict (modify', tell)
import           Crypto.Hash              (SHA256 (..), hashWith)
import qualified Data.Text                as DT
import qualified RIO.Char                 as C
import           RIO.FilePath             ((</>))
import qualified RIO.Text                 as T
import           RIO

import           Hsed.Spec.Anchor         (AnchorId, renderAnchorId)
import           Hsed.Spec.Check
import           Hsed.Spec.Common         (ErrataSet, Finding, Severity (..), SpecId (..), finding)
import qualified Hsed.Spec.Gen            as Gen
import           Hsed.Spec.IR             (CellIR (..), SpecIR (..), StructPath (..), TableIR (..))
import           Hsed.Spec.Rules          (locatedAt, tablePage)


-- | What @hsedc compile@ writes, with everything it found.
data CompileResult = CompileResult
  { crModuleName :: !Text
  , crModuleText :: !Text
  , crDecls      :: ![Gen.Decl]
  , crExcluded   :: ![(AnchorId, Text)]
  , crFindings   :: ![Finding]            -- ^ 'sortFindings' order
  }

-- | @Hsed.Gen.Core.Types@ for @core-2.01@.
generatedModuleName :: SpecId -> Text
generatedModuleName sid = "Hsed.Gen." <> capitalise (T.takeWhile C.isAlpha (specIdText sid)) <> ".Types"
  where capitalise t = maybe t (\(c, cs) -> T.cons (C.toUpper c) (T.toLower cs)) (T.uncons t)

-- | Where the module goes under an output directory: @DIR/Hsed/Gen/Core/Types.hs@.
generatedModulePath :: FilePath -> SpecId -> FilePath
generatedModulePath dir sid = dir </> T.unpack (DT.replace "." "/" (generatedModuleName sid)) <> ".hs"

-- | The checks, then a declaration per bound table of the M1 shapes.
compileSpec :: Compile ()
compileSpec = do
  checks
  env <- ask
  let rows = rights (typeRowReads env)
  forM_ rows $ \tr -> either (exclude (trAnchor tr) (trTable tr)) (emit . Gen.TypeRow) (typeRowDecl env tr)
  forM_ (rights (enumReads env)) $ \er ->
    either (exclude (erAnchor er) (erTable er)) (emit . Gen.Enum) (enumDecl env er (checkEnum env rows er))
  where
    emit :: Gen.Decl -> Compile ()
    emit d = modify' (<> Artefacts [d] [])
    exclude :: AnchorId -> TableIR -> Text -> Compile ()
    exclude aid t why = do
      env <- ask
      modify' (<> Artefacts [] [(aid, why)])
      tell [locatedAt (ceSpec env) (renderAnchorId aid) (fromMaybe 1 (tablePage t))
              (finding Warning "W-GEN-EXCLUDED" ("left out of " <> generatedModuleName (ceSpec env) <> ": " <> why))]

-- | Compile one IR under one errata set.
compileIR :: ErrataSet -> SpecIR -> Either Finding CompileResult
compileIR es ir = do
  ((), arts, fs) <- runCompile (mkCompileEnv es ir) compileSpec
  let name = generatedModuleName (SpecId ir.spec)
  pure CompileResult
    { crModuleName = name
    , crModuleText = Gen.renderModule name (artDecls arts)
    , crDecls      = artDecls arts
    , crExcluded   = artExcluded arts
    , crFindings   = sortFindings fs }

-- | A type row's declaration, or why it has none.
typeRowDecl :: CompileEnv -> TypeRowRead -> Either Text Gen.TypeRowDecl
typeRowDecl env tr = case trUID tr of
  Nothing -> Left ("the UID cell " <> tshow (trUIDText tr) <> " is not eight bytes")
  Just bs -> Right Gen.TypeRowDecl
    { Gen.name       = trName tr
    , Gen.uidBytes   = bs
    , Gen.formatText = trFormat tr
    , Gen.typeDef    = either (const Nothing) Just (trTypeDef tr)
    , Gen.provenance = provenanceOf env (trAnchor tr) (trTable tr) (trCaption tr) }

-- | An enumeration's declaration from its C-ENUM-RANGE result, or why it has none.
enumDecl :: CompileEnv -> EnumRead -> EnumCheck -> Either Text Gen.EnumDecl
enumDecl env er ec = case ecDecl ec of
  Nothing -> Left "C-ENUM-RANGE failed"
  Just (name, ctors, uidBytes, rng) -> Right Gen.EnumDecl
    { Gen.name       = name
    , Gen.ctors      = ctors
    , Gen.typeUID    = uidBytes
    , Gen.range      = rng
    , Gen.provenance = provenanceOf env (erAnchor er) (erTable er) (erCaption er) }

-- | Where a declaration comes from (design example E2).
provenanceOf :: CompileEnv -> AnchorId -> TableIR -> Text -> Gen.Provenance
provenanceOf env aid t caption = Gen.Provenance
  { Gen.spec        = ceSpec env
  , Gen.tableId     = renderAnchorId aid
  , Gen.caption     = caption
  , Gen.pdfPage1    = page
  , Gen.printedPage = page - (ceIR env).labelOffset
  , Gen.ordinal     = fromMaybe 0 t.ordinal
  , Gen.structPath  = let StructPath sp = t.structPath in sp
  , Gen.cellsHash   = cellsHash t }
  where page = fromMaybe 1 (tablePage t)

-- | sha256 over a table's cell texts, header rows first, a tab between cells and
--   a newline between rows: the manifest's @hash@ (@tools/extract/annotate.py@).
cellsHash :: TableIR -> Text
cellsHash t = tshow (hashWith SHA256 (encodeUtf8 joined))
  where
    joined = T.intercalate "\n" [ T.intercalate "\t" (map (.text) row) | row <- t.headerRows <> t.rows ]

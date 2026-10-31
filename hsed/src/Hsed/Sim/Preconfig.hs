{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Sim.Preconfig
Description : An SP's tables folded from its preconfiguration: each table's description and rows, the cells printed over each column's default
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

What an SP holds when it is created, folded from the SSC's preconfiguration
tables ("Hsed.Core.Preconfig"'s 'PreconfTable': Opal 2.30 4.2-4.3, PDF
35-89) into the simulator's form ("Hsed.Sim.Types".@SPState@): each table's
description and its rows, both by the table's UID (Core Table 240's UID of
Table), every row its cells by column number (design docs/spec-as-source.md
sections 8.1 and 8.3; M4b).

The fold reads a run's parameters and nothing of the state, so that the
initial state can run it ("Hsed.Sim.Types".@initialState@); it imports the
parameters ("Hsed.Sim.Params") and hsed-core, and the Admin SP's
hand-written rows ("Hsed.Sim.AdminSP"), its stand-in until M4b's generated
preconfiguration, never a module that imports the state's types. As M4b's
scaffold has it, 'adminSPTables' is the stand-in and 'foldTables' keeps each
plain row's printed cells over its table's defaults; the cells the TPer
chooses ('vendorCell'), the templates' instances ('instances'), the rows of
every object of a table and the parameters' overrides are the fold's next
step (the M4b plan's group G4), and with M4b's integration the tables are the
generated Opal ones.
-}
module Hsed.Sim.Preconfig
  ( adminSPTables
  , foldTables
  , vendorCell
  , instances
  ) where

import qualified RIO.List            as L
import qualified RIO.Map             as Map
import           RIO

import           Hsed.Core.Preconfig (Placeholder, PreCell (..), PreconfRow (..), PreconfTable (..), RowKind (..))
import           Hsed.Core.Table     (Cell, ColNum, Pre (..), Row, TableDesc (..))
import           Hsed.Core.UID       (UID)
import           Hsed.Sim.AdminSP    (handTables)
import           Hsed.Sim.Params     (SimParams)


-- | The Admin SP's preconfiguration tables for a run: until M4b's generated
--   Opal tables (Opal 2.30 Tables 18-25, 27-29 and 33, PDF 35-52), the
--   fourteen hand-written rows of six of them, the cells the parameters
--   choose already filled ("Hsed.Sim.AdminSP".'handTables').
adminSPTables :: SimParams -> [PreconfTable]
adminSPTables = handTables

-- | An SP's tables folded from its preconfiguration tables: each table's
--   description and its rows, both by the table's UID ('tdTableUID'), the
--   rows in the order the tables give them. A row holds the cells its table
--   prints for it ('Given') and, for every column it leaves blank, the
--   column's default ('ptDefaults'), in column order: a column a row lacks
--   is one a Get does not return, which a host reads as one it may not read
--   (Core 5.3.4.2.2, PDF 223; design 13.31 (a)).
--
--   As M4b's scaffold has it, only the plain rows are folded and only their
--   printed cells kept, which is all the stand-in has ('adminSPTables'): a
--   cell the TPer chooses ('VendorUnique', 'vendorCell'), a template's
--   instances ('Template', 'instances'), the rows of every object of a table
--   ('EveryObject') and the parameters' overrides (the Locking SP's Original
--   Factory State, TPER_RESET's enabling) are the M4b plan's group G4's.
foldTables :: SimParams -> [PreconfTable] -> (Map UID TableDesc, Map UID [Row])
foldTables _ tables =
  ( Map.fromList [ (tdTableUID (ptDesc t), ptDesc t) | t <- tables ]
  , Map.fromListWith (flip (<>)) [ (tdTableUID (ptDesc t), mapMaybe (folded (ptDefaults t) . row) (ptRows t)) | t <- tables ]
  )
  where
    folded defaults pr = case prKind pr of
      Plain -> Just (overDefaults defaults [ (c, cell) | (c, Given cell) <- prCells pr ])
      _     -> Nothing
    -- the cells a row gives, and the default of every column it does not, in column order
    overDefaults defaults given = L.sortOn fst (given <> [ d | d@(c, _) <- defaults, c `notElem` map fst given ])

-- | The cell the TPer chooses where a preconfiguration table leaves a value
--   to it ('VendorUnique': @VU@, Opal 2.30 Table 1, PDF 12, or @MSID@,
--   Table 25, PDF 48), from the run's parameters: in a table of a
--   description, in a row (its cells so far, which name the object), in a
--   column, for the marker printed; 'Nothing' for a cell it does not name.
--   Each value cites its source (design 13.17): C_PIN_SID's PIN the SID's
--   PIN or else the MSID (Opal 2.30 4.2.1.8, PDF 49), C_PIN_MSID's the MSID,
--   C_PIN_SID's TryLimit the parameters' and its Tries 0 (Core 5.3.4.1.1.2,
--   PDF 210), TPerInfo's GUDID @spGudid@ (Opal 2.30 *TP1, PDF 50), and the
--   others of Opal 2.30 Tables 28 and 33 (PDF 50-52). The M4b plan's group
--   G4 writes it.
vendorCell :: SimParams -> TableDesc -> Row -> ColNum -> Text -> Maybe Cell
vendorCell = error "G4: vendorCell"

-- | The instances a template row of a table takes beside the ones its
--   table prints, by number: the Admin SP's Authority and C_PIN tables under
--   @XX@ the Admin authorities after Admin1, 2 to @spAdminSPAdmins@ (Opal 2.30
--   PDF 47: Admin1 (M), any more (O); Tables 24-25, PDF 48-49). The M4b plan's
--   group G4 writes it.
instances :: SimParams -> TableDesc -> Placeholder -> [Natural]
instances = error "G4: instances"

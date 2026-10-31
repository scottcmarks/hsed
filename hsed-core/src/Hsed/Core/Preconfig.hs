{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Hsed.Core.Preconfig
Description : The runtime form of an SSC's preconfiguration table: printed cells, vendor-unique cells, template and every-object rows
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

An SSC's preconfiguration tables give the rows each of its SPs holds when the
SP is created (Opal 2.30 sections 4.2-4.3, PDF 35-89: the Admin SP's Tables
18-25, 27-29 and 33, the Locking SP's 34-42 and 44-48), and more than a Core
table of values ("Hsed.Core.Table"'s 'Row') can say:

* a cell may leave its value to the TPer, @VU@ (Opal Table 1, PDF 12), or
  print @MSID@ where C_PIN_MSID's PIN is the TPer's MSID (Table 25, PDF 48):
  a 'VendorUnique' cell, beside the 'Given' cells the table prints;
* a row may be a template, its UID printed as a base and a placeholder,
  @00 00 00 09 00 00 02 00 (+XX)@, its names and references carrying the
  placeholder's letters, @AdminXX@ and @C_PIN_AdminXX@ (Table 24, PDF 48):
  one row for each instance the TPer holds, Admin1 printed and any additional
  Admin optional (PDF 47); the Locking SP's @(+XX XX)@, @(+MM MM)@ and
  @(+NNNN)@ number two bytes (Tables 38-41, PDF 56-84);
* a row's InvokingID may be @TT TT TT TT@ after a table's half-UID, a
  shorthand for the lower bytes of every object of that table (Table 22's
  footnotes *AC1-*AC7, PDF 38-39): one AccessControl row for each object;
* a row marked @(O)@ is optional (Tables 22 and 23, PDF 38 and 46), and a
  row's footnote markers (@*AC1@, @*ACE1@, …) are its notes.

A 'PreconfTable' is one such table: the description of the table it
preconfigures, each column's default, which a row that does not give the
column keeps (a column a Get does not return reads as one the host may not
read, Core 5.3.4.2.2, PDF 223, so every column holds a value; design
docs/spec-as-source.md 13.31 (a)), and its rows, each a 'PreconfRow' in a
'Hsed.Core.Table.Pre' with the access its cells have in the SSC's legend. The spec compiler generates one per table; the
simulator folds them into an SP's tables, filling the vendor-unique cells from
a run's parameters, instantiating the templates ('instanceUID',
'instantiate') and expanding the every-object rows over the SP's objects
(design section 8.3).
-}
module Hsed.Core.Preconfig
  ( -- * Cells
    PreCell (..)
    -- * Rows
  , Placeholder (..)
  , RowKind (..)
  , PreconfRow (..)
    -- * Tables
  , PreconfTable (..)
    -- * Template instances
  , instanceUID
  , instantiate
  ) where

import qualified GHC.Exts                   as Exts (toList)
import qualified RIO.ByteString             as B
import qualified RIO.List                   as L
import qualified RIO.Text                   as T
import qualified RIO.Text.Partial           as T (replace)
import           RIO

import           Hsed.Core.Table            (Cell (..), ColNum, Pre, Row, TableDesc)
import           Hsed.Core.Token            (IsToken (..), Token (..))
import           Hsed.Core.UID              (HalfUID, UID)


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Hsed.Core.UID (uid)


-- * Cells

-- | One cell of a preconfiguration table.
data PreCell
  = Given !Cell          -- ^ the value the table prints, read as its column's type says
  | VendorUnique !Text   -- ^ a value the TPer chooses, the marker as printed: @VU@ (Opal Table 1, PDF 12), or @MSID@ (C_PIN_MSID's PIN, Table 25, PDF 48)
  deriving (Eq, Show)


-- * Rows

-- | A template row's placeholder: the letters its names and references
--   carry, and how many of its UID's last bytes number the instance.
data Placeholder = Placeholder
  { phLetters :: !Text   -- ^ as the names print them: @XX@ (@AdminXX@); @XXXX@ for @(+XX XX)@ (@AdminXXXX@); @MMMM@; @NNNN@
  , phBytes   :: !Int    -- ^ @XX@: 1; @XX XX@, @XXXX@, @MMMM@, @NNNN@: 2
  }
  deriving (Eq, Show)

-- | What a row of a preconfiguration table stands for.
data RowKind
  = Plain                             -- ^ one row, as printed
  | Template !Placeholder ![ColNum]   -- ^ @(+XX)@: a row for each instance, the columns listed carrying the placeholder (the UID or InvokingID cell, names, references)
  | EveryObject !HalfUID              -- ^ @TT TT TT TT@ after this half-UID (Opal *AC1-*AC7, PDF 38-39): a row for each object of the table whose objects' UIDs begin with it
  deriving (Eq, Show)

-- | One row of a preconfiguration table: what it stands for, whether it is
--   optional, the cells it prints by column number, in order (a column it
--   leaves blank is absent), and its notes.
data PreconfRow = PreconfRow
  { prKind     :: !RowKind
  , prOptional :: !Bool                  -- ^ marked @(O)@ (Opal Tables 22 and 23, PDF 38 and 46; additional Admins, PDF 47)
  , prCells    :: ![(ColNum, PreCell)]
  , prNotes    :: ![Text]                -- ^ the row's footnote markers and notes, as printed: @*AC1@, @*ACE1@, …
  }
  deriving (Eq, Show)


-- * Tables

-- | One preconfiguration table: the table it preconfigures, each column's
--   default, and its rows in the table's order.
data PreconfTable = PreconfTable
  { ptDesc     :: !TableDesc            -- ^ the description of the table preconfigured, with the SSC's columns added
  , ptDefaults :: !Row                  -- ^ each column's default by column number, which a row that does not give the column keeps
  , ptRows     :: ![Pre PreconfRow]     -- ^ each row with the access its cells have in the SSC's legend
  }
  deriving (Eq, Show)


-- * Template instances

-- | The UID of a template's instance: the base UID with the number its last
--   'phBytes' bytes hold increased by the instance's number, the template
--   reading "the base plus XX"; Nothing where the sum does not fit those
--   bytes (a count of bytes outside 0 to 8 is read as the nearer bound).
--   Table 24's AdminXX (Opal 2.30, PDF 48) at 2:
--
-- >>> instanceUID (Placeholder "XX" 1) (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x00) 2
-- Just uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x02
--
--   Table 40's AdminXXXX, @(+XX XX)@ (PDF 83), at 1, which is the Admin1 the
--   table prints (PDF 82):
--
-- >>> instanceUID (Placeholder "XXXX" 2) (uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 0x00) 1
-- Just uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 0x01
--
--   The number is added, not written over the bytes: Table 39's
--   ACE_C_PIN_UserMMMM_Set_PIN, @00 00 00 08 00 03 A8 00 (+MMMM)@ (PDF 79), at 1:
--
-- >>> instanceUID (Placeholder "MMMM" 2) (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xA8 0x00) 1
-- Just uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa8 0x01
--
--   One byte numbers 255 more instances, not 256:
--
-- >>> map (instanceUID (Placeholder "XX" 1) (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0x00)) [255, 256]
-- [Just uid 0x00 0x00 0x00 0x09 0x00 0x00 0x02 0xff,Nothing]
instanceUID :: Placeholder -> UID -> Natural -> Maybe UID
instanceUID ph u n
  | total < 256 ^ width = fromToken (Bytes (B.pack (upper <> bigEndian width total)))
  | otherwise           = Nothing
  where
    width          = max 0 (min 8 (phBytes ph))
    (upper, lower) = L.splitAt (8 - width) (Exts.toList u)
    total          = L.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 lower + n

-- | A number in so many bytes, most significant first.
bigEndian :: Int -> Natural -> [Word8]
bigEndian width v = [ fromIntegral ((v `div` (256 ^ i)) `mod` 256) | i <- [width - 1, width - 2 .. 0] ]

-- | A template row's cells for one instance. In the columns listed, a uid is
--   the instance's ('instanceUID'), a reference is to the instance of the
--   template it names, and a name or a reference has the placeholder's
--   letters replaced by the instance's number; in a list, only the elements
--   whose names carry the letters change (Table 39's BooleanExpr
--   @Admins OR UserMMMM@, PDF 79). The other columns, and a cell the TPer
--   chooses, are as printed; so is a uid the placeholder's bytes cannot hold
--   the instance's number in (instanceUID's Nothing), the caller numbering
--   only the instances they hold. Table 24's AdminXX at 2 is Admin2, whose
--   Credential is C_PIN_AdminXX's base plus 2 (PDF 48-49):
--
-- >>> let credential = Given (RefCell "C_PIN_AdminXX" (uid 0x00 0x00 0x00 0x0B 0x00 0x00 0x02 0x00))
-- >>> instantiate (Placeholder "XX" 1) [0x01, 0x0A] 2 [(0x01, Given (TextCell "AdminXX")), (0x0A, credential)]
-- [(1,Given (TextCell "Admin2")),(10,Given (RefCell "C_PIN_Admin2" uid 0x00 0x00 0x00 0x0b 0x00 0x00 0x02 0x02))]
instantiate :: Placeholder -> [ColNum] -> Natural -> [(ColNum, PreCell)] -> [(ColNum, PreCell)]
instantiate ph cols n = map (\(c, pc) -> (c, if c `elem` cols then inColumn pc else pc))
  where
    inColumn = \case
      Given cell -> Given (carried cell)
      vu         -> vu
    -- a cell of a column that carries the placeholder
    carried = \case
      UIDCell u   -> UIDCell (step u)
      RefCell t u -> RefCell (rename t) (step u)
      TextCell t  -> TextCell (rename t)
      ListCell cs -> ListCell (map element cs)
      cell        -> cell
    -- an element of a list in such a column: the elements whose names carry the letters
    element = \case
      RefCell t u | carries t -> RefCell (rename t) (step u)
      TextCell t  | carries t -> TextCell (rename t)
      ListCell cs             -> ListCell (map element cs)
      cell                    -> cell
    carries t = not (T.null (phLetters ph)) && phLetters ph `T.isInfixOf` t
    rename t
      | carries t = T.replace (phLetters ph) (tshow n) t
      | otherwise = t
    step u = fromMaybe u (instanceUID ph u n)

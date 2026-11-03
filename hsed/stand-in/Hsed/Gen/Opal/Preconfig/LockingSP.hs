{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hsed.Gen.Opal.Preconfig.LockingSP
Description : The Locking SP's preconfiguration: each table's description, its columns' defaults and its rows, generated from opal-2.30
Copyright   : (c) Magnolia Heights R&D, 2026
License     : BSD3
Maintainer  : scott@magnolia-heights.com

STAND-IN: replaced by make gen at M5a integration. This is not hsedc's
output for the committed sidecar (which binds none of the Locking SP's tables
until M5a's readings and annotations land): it is the M5 tables survey's
scratch compile of Opal 2.30 Tables 34-42 and 44-48 (PDF 52-89) with those
readings simulated, kept under @hsed/stand-in@, outside the directory hsedc
compile owns (@hsed/gen/Hsed/Gen/Opal@, which it would empty of it), so that
the simulator can fold the Locking SP before the compiler emits it. Seven
cells are hand-edited to hsed-core's preconfiguration cells of M5a's
scaffold, each marked STAND-IN where it is: Table 36's MBR and DataStore
Rows ('VendorAtLeast', PDF 54), Table 41's C_PIN_Admin1 PIN, "SID or MSID"
and its note 1 (PDF 84), Table 44's MaxRanges, 8 and its note 1, and Table
45's three ActiveKey cells, K_AES_128[256] (*LT1) (PDF 86). The integration
diffs this module against the generated one and explains every hunk before
deleting it. The tables are TCG's text: see tcg/TCG-NOTICE.txt.

The survey's compiler wrote, as hsedc does: each definition names its table,
its PDF and printed page, its ordinal (a table's in the structure tree, ord;
a signature block's, a paragraph's or a heading's among the IR's, so named)
and the sha256 of its cells (docs/spec-as-source.md section 3.5). Import it
qualified: "Hsed.Gen.Opal.Preconfig.AdminSP" names uAuthority_Admin1 and
uC_PIN_Admin1 too, at other UIDs.
-}
module Hsed.Gen.Opal.Preconfig.LockingSP where

import           Hsed.Core.Preconfig      (Placeholder (..), PreCell (..), PreconfRow (..), PreconfTable (..), RowKind (..))
import           Hsed.Core.Table          (Access (..), Cell (..), Pre (..))
import           Hsed.Core.UID            (halfUID, UID, uid)
import           Hsed.Gen.Core.TableDescs (accessControlTableDesc, aceTableDesc, authorityTableDesc, c_PINTableDesc, k_AES_128TableDesc, k_AES_256TableDesc, lockingTableDesc, mbrControlTableDesc, methodIDTableDesc, secretProtectTableDesc, spInfoTableDesc, spTemplatesTableDesc)
import           Hsed.Gen.Opal.TableDescs (lockingInfoTableDesc, tableTableDesc)
import           Prelude                  (Bool (..))

-- | opal-2.30 T34 - Locking SP - SPInfo Table Preconfiguration (PDF 52 / p.51, ord 36, sha256:f44d2a702e6e): the SPInfo table (core-2.01 T169) as the Locking SP holds it
lockingSPSPInfoTablePreconfiguration :: PreconfTable
lockingSPSPInfoTablePreconfiguration = PreconfTable
  { ptDesc     = spInfoTableDesc
  , ptDefaults =
      [ (0x01, UIDCell (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- SPID
      , (0x02, TextCell "")  -- Name
      , (0x03, UIntCell 0)  -- Size
      , (0x04, UIntCell 0)  -- SizeInUse
      , (0x05, UIntCell 0)  -- SPSessionTimeout
      , (0x06, EnumCell "False" 0)  -- Enabled
      ]
  , ptRows     =
      [ Pre  -- row 1 Locking
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x02 0x00 0x00 0x00 0x01)))  -- UID
            , (0x01, Given (UIDCell (uid 0x00 0x00 0x02 0x05 0x00 0x00 0x00 0x02)))  -- SPID
            , (0x02, Given (TextCell "Locking"))  -- Name
            , (0x06, Given (EnumCell "T" 1))  -- Enabled
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, ReadOnlySpecified), (0x02, Unspecified), (0x03, Unspecified)
          , (0x04, Unspecified), (0x05, Unspecified), (0x06, ReadOnlySpecified)
          ]
      ]
  }

-- | opal-2.30 T35 - Locking SP - SPTemplates Table Preconfiguration (PDF 53 / p.52, ord 37, sha256:b6655303377f): the SPTemplates table (core-2.01 T170) as the Locking SP holds it
lockingSPSPTemplatesTablePreconfiguration :: PreconfTable
lockingSPSPTemplatesTablePreconfiguration = PreconfTable
  { ptDesc     = spTemplatesTableDesc
  , ptDefaults =
      [ (0x01, RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- TemplateID
      , (0x02, TextCell "")  -- Name
      , (0x03, BytesCell "\NUL\NUL\NUL\NUL")  -- Version
      ]
  , ptRows     =
      [ Pre  -- row 1 Base; notes *SP1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x03 0x00 0x00 0x00 0x01)))  -- UID
            , (0x01, Given (RefCell "Base" (uid 0x00 0x00 0x02 0x04 0x00 0x00 0x00 0x01)))  -- TemplateID
            , (0x02, Given (TextCell "Base"))  -- Name
            , (0x03, Given (BytesCell "\NUL\NUL\NUL\STX"))  -- Version
            ]
            ["*SP1"])
          [ (0x00, ReadOnlySpecified), (0x01, ReadOnlySpecified), (0x02, Unspecified), (0x03, ReadOnlySpecified)
          ]
      , Pre  -- row 2 Locking; notes *SP1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x03 0x00 0x00 0x00 0x06)))  -- UID
            , (0x01, Given (RefCell "Locking" (uid 0x00 0x00 0x02 0x04 0x00 0x00 0x00 0x06)))  -- TemplateID
            , (0x02, Given (TextCell "Locking"))  -- Name
            , (0x03, Given (BytesCell "\NUL\NUL\NUL\STX"))  -- Version
            ]
            ["*SP1"])
          [ (0x00, ReadOnlySpecified), (0x01, ReadOnlySpecified), (0x02, Unspecified), (0x03, ReadOnlySpecified)
          ]
      ]
  }

-- | opal-2.30 T36 - Locking SP - Table Table Preconfiguration (PDF 53 / p.52, ord 38, sha256:39c4cb6305f4): the Table table (core-2.01 T171, with opal-2.30's columns) as the Locking SP holds it
lockingSPTableTablePreconfiguration :: PreconfTable
lockingSPTableTablePreconfiguration = PreconfTable
  { ptDesc     = tableTableDesc
  , ptDefaults =
      [ (0x01, TextCell "")  -- Name
      , (0x02, TextCell "")  -- CommonName
      , (0x03, RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- TemplateID
      , (0x04, EnumCell "Object" 1)  -- Kind
      , (0x05, RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- Column
      , (0x06, UIntCell 0)  -- NumColumns
      , (0x07, UIntCell 0)  -- Rows
      , (0x08, UIntCell 0)  -- RowsFree
      , (0x09, UIntCell 0)  -- RowBytes
      , (0x0A, UIDCell (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- LastID
      , (0x0B, UIntCell 0)  -- MinSize
      , (0x0C, UIntCell 0)  -- MaxSize
      , (0x0D, UIntCell 0)  -- MandatoryWriteGranularity
      , (0x0E, UIntCell 0)  -- RecommendedAccessGranularity
      ]
  , ptRows     =
      [ Pre  -- row 1 Table
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x01)))  -- UID
            , (0x01, Given (TextCell "Table"))  -- Name
            , (0x04, Given (EnumCell "Object" 1))  -- Kind
            , (0x0D, Given (UIntCell 0))  -- MandatoryWriteGranularity
            , (0x0E, Given (UIntCell 0))  -- RecommendedAccessGranularity
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, ReadOnlySpecified), (0x0E, ReadOnlySpecified)
          ]
      , Pre  -- row 2 SPInfo
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x02)))  -- UID
            , (0x01, Given (TextCell "SPInfo"))  -- Name
            , (0x04, Given (EnumCell "Object" 1))  -- Kind
            , (0x0D, Given (UIntCell 0))  -- MandatoryWriteGranularity
            , (0x0E, Given (UIntCell 0))  -- RecommendedAccessGranularity
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, ReadOnlySpecified), (0x0E, ReadOnlySpecified)
          ]
      , Pre  -- row 3 SPTemplates
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x03)))  -- UID
            , (0x01, Given (TextCell "SPTemplates"))  -- Name
            , (0x04, Given (EnumCell "Object" 1))  -- Kind
            , (0x0D, Given (UIntCell 0))  -- MandatoryWriteGranularity
            , (0x0E, Given (UIntCell 0))  -- RecommendedAccessGranularity
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, ReadOnlySpecified), (0x0E, ReadOnlySpecified)
          ]
      , Pre  -- row 4 MethodID
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x06)))  -- UID
            , (0x01, Given (TextCell "MethodID"))  -- Name
            , (0x04, Given (EnumCell "Object" 1))  -- Kind
            , (0x0D, Given (UIntCell 0))  -- MandatoryWriteGranularity
            , (0x0E, Given (UIntCell 0))  -- RecommendedAccessGranularity
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, ReadOnlySpecified), (0x0E, ReadOnlySpecified)
          ]
      , Pre  -- row 5 AccessControl
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x07)))  -- UID
            , (0x01, Given (TextCell "AccessControl"))  -- Name
            , (0x04, Given (EnumCell "Object" 1))  -- Kind
            , (0x0D, Given (UIntCell 0))  -- MandatoryWriteGranularity
            , (0x0E, Given (UIntCell 0))  -- RecommendedAccessGranularity
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, ReadOnlySpecified), (0x0E, ReadOnlySpecified)
          ]
      , Pre  -- row 6 ACE
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x08)))  -- UID
            , (0x01, Given (TextCell "ACE"))  -- Name
            , (0x04, Given (EnumCell "Object" 1))  -- Kind
            , (0x0D, Given (UIntCell 0))  -- MandatoryWriteGranularity
            , (0x0E, Given (UIntCell 0))  -- RecommendedAccessGranularity
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, ReadOnlySpecified), (0x0E, ReadOnlySpecified)
          ]
      , Pre  -- row 7 Authority
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x09)))  -- UID
            , (0x01, Given (TextCell "Authority"))  -- Name
            , (0x04, Given (EnumCell "Object" 1))  -- Kind
            , (0x0D, Given (UIntCell 0))  -- MandatoryWriteGranularity
            , (0x0E, Given (UIntCell 0))  -- RecommendedAccessGranularity
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, ReadOnlySpecified), (0x0E, ReadOnlySpecified)
          ]
      , Pre  -- row 8 C_PIN
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x0b)))  -- UID
            , (0x01, Given (TextCell "C_PIN"))  -- Name
            , (0x04, Given (EnumCell "Object" 1))  -- Kind
            , (0x0D, Given (UIntCell 0))  -- MandatoryWriteGranularity
            , (0x0E, Given (UIntCell 0))  -- RecommendedAccessGranularity
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, ReadOnlySpecified), (0x0E, ReadOnlySpecified)
          ]
      , Pre  -- row 9 SecretProtect
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x1d)))  -- UID
            , (0x01, Given (TextCell "SecretProtect"))  -- Name
            , (0x04, Given (EnumCell "Object" 1))  -- Kind
            , (0x0D, Given (UIntCell 0))  -- MandatoryWriteGranularity
            , (0x0E, Given (UIntCell 0))  -- RecommendedAccessGranularity
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, ReadOnlySpecified), (0x0E, ReadOnlySpecified)
          ]
      , Pre  -- row 10 LockingInfo
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x08 0x01)))  -- UID
            , (0x01, Given (TextCell "LockingInfo"))  -- Name
            , (0x04, Given (EnumCell "Object" 1))  -- Kind
            , (0x0D, Given (UIntCell 0))  -- MandatoryWriteGranularity
            , (0x0E, Given (UIntCell 0))  -- RecommendedAccessGranularity
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, ReadOnlySpecified), (0x0E, ReadOnlySpecified)
          ]
      , Pre  -- row 11 Locking
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x08 0x02)))  -- UID
            , (0x01, Given (TextCell "Locking"))  -- Name
            , (0x04, Given (EnumCell "Object" 1))  -- Kind
            , (0x0D, Given (UIntCell 0))  -- MandatoryWriteGranularity
            , (0x0E, Given (UIntCell 0))  -- RecommendedAccessGranularity
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, ReadOnlySpecified), (0x0E, ReadOnlySpecified)
          ]
      , Pre  -- row 12 MBRControl
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x08 0x03)))  -- UID
            , (0x01, Given (TextCell "MBRControl"))  -- Name
            , (0x04, Given (EnumCell "Object" 1))  -- Kind
            , (0x0D, Given (UIntCell 0))  -- MandatoryWriteGranularity
            , (0x0E, Given (UIntCell 0))  -- RecommendedAccessGranularity
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, ReadOnlySpecified), (0x0E, ReadOnlySpecified)
          ]
      , Pre  -- row 13 MBR
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x08 0x04)))  -- UID
            , (0x01, Given (TextCell "MBR"))  -- Name
            , (0x04, Given (EnumCell "Byte" 2))  -- Kind
            , (0x07, VendorAtLeast 0x08000000 "0x08000000 min")  -- Rows; STAND-IN: hand-edited (VendorAtLeast)
            , (0x0D, VendorUnique "VU")  -- MandatoryWriteGranularity
            , (0x0E, VendorUnique "VU")  -- RecommendedAccessGranularity
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, ReadOnlyVU)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, ReadOnlyVU), (0x0E, ReadOnlyVU)
          ]
      , Pre  -- row 14 K_AES_128; notes *TT1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x08 0x05)))  -- UID
            , (0x01, Given (TextCell "K_AES_128"))  -- Name
            , (0x04, Given (EnumCell "Object" 1))  -- Kind
            , (0x0D, Given (UIntCell 0))  -- MandatoryWriteGranularity
            , (0x0E, Given (UIntCell 0))  -- RecommendedAccessGranularity
            ]
            ["*TT1"])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, ReadOnlySpecified), (0x0E, ReadOnlySpecified)
          ]
      , Pre  -- row 15 K_AES_256; notes *TT1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x08 0x06)))  -- UID
            , (0x01, Given (TextCell "K_AES_256"))  -- Name
            , (0x04, Given (EnumCell "Object" 1))  -- Kind
            , (0x0D, Given (UIntCell 0))  -- MandatoryWriteGranularity
            , (0x0E, Given (UIntCell 0))  -- RecommendedAccessGranularity
            ]
            ["*TT1"])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, ReadOnlySpecified), (0x0E, ReadOnlySpecified)
          ]
      , Pre  -- row 16 DataStore
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x10 0x01)))  -- UID
            , (0x01, Given (TextCell "DataStore"))  -- Name
            , (0x04, Given (EnumCell "Byte" 2))  -- Kind
            , (0x07, VendorAtLeast 0x00A00000 "0x00A00000 min")  -- Rows; STAND-IN: hand-edited (VendorAtLeast)
            , (0x0D, VendorUnique "VU")  -- MandatoryWriteGranularity
            , (0x0E, VendorUnique "VU")  -- RecommendedAccessGranularity
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, ReadOnlyVU)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, ReadOnlyVU), (0x0E, ReadOnlyVU)
          ]
      ]
  }

-- | opal-2.30 T36 - Locking SP - Table Table Preconfiguration (PDF 54 / p.53, ord 38, sha256:39c4cb6305f4), row 16: DataStore
uTable_DataStore :: UID
uTable_DataStore = uid 0x00 0x00 0x00 0x01 0x00 0x00 0x10 0x01

-- | opal-2.30 T37 - Locking SP - MethodID Table Preconfiguration (PDF 55 / p.54, ord 39, sha256:a9d4ceefa6dd): the MethodID table (core-2.01 T174) as the Locking SP holds it
lockingSPMethodIDTablePreconfiguration :: PreconfTable
lockingSPMethodIDTablePreconfiguration = PreconfTable
  { ptDesc     = methodIDTableDesc
  , ptDefaults =
      [ (0x01, TextCell "")  -- Name
      , (0x02, TextCell "")  -- CommonName
      , (0x03, RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- TemplateID
      ]
  , ptRows     =
      [ Pre  -- row 1 Next
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x08)))  -- UID
            , (0x01, Given (TextCell "Next"))  -- Name
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          ]
      , Pre  -- row 2 GetACL
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x0d)))  -- UID
            , (0x01, Given (TextCell "GetACL"))  -- Name
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          ]
      , Pre  -- row 3 GenKey
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x10)))  -- UID
            , (0x01, Given (TextCell "GenKey"))  -- Name
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          ]
      , Pre  -- row 4 RevertSP; notes *MT1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x11)))  -- UID
            , (0x01, Given (TextCell "RevertSP"))  -- Name
            ]
            ["*MT1"])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          ]
      , Pre  -- row 5 Get
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- UID
            , (0x01, Given (TextCell "Get"))  -- Name
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          ]
      , Pre  -- row 6 Set
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- UID
            , (0x01, Given (TextCell "Set"))  -- Name
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          ]
      , Pre  -- row 7 Authenticate
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x1c)))  -- UID
            , (0x01, Given (TextCell "Authenticate"))  -- Name
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          ]
      , Pre  -- row 8 Random
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x06 0x01)))  -- UID
            , (0x01, Given (TextCell "Random"))  -- Name
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, Unspecified)
          ]
      ]
  }

-- | opal-2.30 T37 - Locking SP - MethodID Table Preconfiguration (PDF 55 / p.54, ord 39, sha256:a9d4ceefa6dd), row 4: RevertSP
uMethodID_RevertSP :: UID
uMethodID_RevertSP = uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x11

-- | opal-2.30 T38 - Locking SP - AccessControl Table Preconfiguration (PDF 56 / p.55, ord 40, sha256:2c8c7d992ab3): the AccessControl table (core-2.01 T175) as the Locking SP holds it
lockingSPAccessControlTablePreconfiguration :: PreconfTable
lockingSPAccessControlTablePreconfiguration = PreconfTable
  { ptDesc     = accessControlTableDesc
  , ptDefaults =
      [ (0x01, UIDCell (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- InvokingID
      , (0x02, RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- MethodID
      , (0x03, TextCell "")  -- CommonName
      , (0x04, ListCell [])  -- ACL
      , (0x05, EnumCell "None" 0)  -- Log
      , (0x06, ListCell [])  -- AddACEACL
      , (0x07, ListCell [])  -- RemoveACEACL
      , (0x08, ListCell [])  -- GetACLACL
      , (0x09, ListCell [])  -- DeleteMethodACL
      , (0x0A, EnumCell "None" 0)  -- AddACELog
      , (0x0B, EnumCell "None" 0)  -- RemoveACELog
      , (0x0C, EnumCell "None" 0)  -- GetACLLog
      , (0x0D, EnumCell "None" 0)  -- DeleteMethodLog
      , (0x0E, RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- LogTo
      ]
  , ptRows     =
      [ Pre  -- row 2 (group SP)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Authenticate" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x1c)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 3 (group SP)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Random" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x06 0x01)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 4 (group SP); notes *AC1
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "RevertSP" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x11)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Admin" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x02)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            ["*AC1"])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 6 (group Table)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Next" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x08)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 7 (group Table); every object of the table 00 00 00 01; notes *AC2
          (PreconfRow (EveryObject (halfUID 0x00 0x00 0x00 0x01)) False
            [ (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            ["*AC2"])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 9 (group SPInfo)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x02 0x00 0x00 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 11 (group SPTemplates)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x03 0x00 0x00 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Next" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x08)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 12 (group SPTemplates); every object of the table 00 00 00 03; notes *AC3
          (PreconfRow (EveryObject (halfUID 0x00 0x00 0x00 0x03)) False
            [ (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            ["*AC3"])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 14 (group MethodID)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Next" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x08)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 15 (group MethodID); every object of the table 00 00 00 06; notes *AC4
          (PreconfRow (EveryObject (halfUID 0x00 0x00 0x00 0x06)) False
            [ (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            ["*AC4"])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 17 (group ACE)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Next" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x08)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 18 (group ACE); every object of the table 00 00 00 08; notes *AC5
          (PreconfRow (EveryObject (halfUID 0x00 0x00 0x00 0x08)) False
            [ (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Get_All" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x00)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            ["*AC5"])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 19 (group ACE)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 20 (group ACE)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x90 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 21 (group ACE)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa8 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 22 (group ACE); a template, MMMM
          (PreconfRow (Template (Placeholder "MMMM" 2) [0x01]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa8 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 23 (group ACE)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x04 0x40 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 24 (group ACE); a template, MMMM
          (PreconfRow (Template (Placeholder "MMMM" 2) [0x01]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x04 0x40 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 25 (group ACE); notes *AC6
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb0 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            ["*AC6"])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 26 (group ACE); notes *AC6
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb0 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            ["*AC6"])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 27 (group ACE); a template, NNNN; notes *AC6
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x01]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb0 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            ["*AC6"])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 28 (group ACE); notes *AC6
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb8 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            ["*AC6"])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 29 (group ACE); notes *AC6
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb8 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            ["*AC6"])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 30 (group ACE); a template, NNNN; notes *AC6
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x01]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb8 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            ["*AC6"])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 31 (group ACE)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xd0 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 32 (group ACE)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xd0 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 33 (group ACE); a template, NNNN
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x01]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xd0 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 34 (group ACE)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe0 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 35 (group ACE)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe0 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 36 (group ACE); a template, NNNN
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x01]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe0 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 37 (group ACE)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe8 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 38 (group ACE)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe8 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 39 (group ACE); a template, NNNN
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x01]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe8 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 40 (group ACE)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xf8 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 41 (group ACE)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xfc 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 42 (group ACE)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xfc 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_ACE_Set_BooleanExpression" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 44 (group Authority)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Next" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x08)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 45 (group Authority); every object of the table 00 00 00 09; notes *AC7
          (PreconfRow (EveryObject (halfUID 0x00 0x00 0x00 0x09)) False
            [ (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Authority_Get_All" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x90 0x00), RefCell "ACE_Anybody_Get_CommonName" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x03)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            ["*AC7"])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 46 (group Authority)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Admins_Set_CommonName" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x04)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 47 (group Authority)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 0x02)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Authority_Set_Enabled" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x90 0x01), RefCell "ACE_Admins_Set_CommonName" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x04)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 48 (group Authority); a template, XXXX
          (PreconfRow (Template (Placeholder "XXXX" 2) [0x01]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Authority_Set_Enabled" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x90 0x01), RefCell "ACE_Admins_Set_CommonName" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x04)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 49 (group Authority)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x03 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Authority_Set_Enabled" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x90 0x01), RefCell "ACE_User1_Set_CommonName" (uid 0x00 0x00 0x00 0x08 0x00 0x04 0x40 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 50 (group Authority); a template, MMMM
          (PreconfRow (Template (Placeholder "MMMM" 2) [0x01, 0x04]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x03 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Authority_Set_Enabled" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x90 0x01), RefCell "ACE_UserMMMM_Set_CommonName" (uid 0x00 0x00 0x00 0x08 0x00 0x04 0x40 0x00)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 52 (group C_PIN)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x0b 0x00 0x00 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Next" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x08)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 53 (group C_PIN)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x0b 0x00 0x01 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_C_PIN_Admins_Get_All_NOPIN" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa0 0x00)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 54 (group C_PIN); a template, XXXX
          (PreconfRow (Template (Placeholder "XXXX" 2) [0x01]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x0b 0x00 0x01 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_C_PIN_Admins_Get_All_NOPIN" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa0 0x00)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 55 (group C_PIN)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x0b 0x00 0x03 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_C_PIN_Admins_Get_All_NOPIN" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa0 0x00)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 56 (group C_PIN); a template, MMMM
          (PreconfRow (Template (Placeholder "MMMM" 2) [0x01]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x0b 0x00 0x03 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_C_PIN_Admins_Get_All_NOPIN" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa0 0x00)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 57 (group C_PIN)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x0b 0x00 0x01 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_C_PIN_Admins_Set_PIN" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa0 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 58 (group C_PIN); a template, XXXX
          (PreconfRow (Template (Placeholder "XXXX" 2) [0x01]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x0b 0x00 0x01 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_C_PIN_Admins_Set_PIN" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa0 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 59 (group C_PIN)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x0b 0x00 0x03 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_C_PIN_User1_Set_PIN" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa8 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 60 (group C_PIN); a template, MMMM
          (PreconfRow (Template (Placeholder "MMMM" 2) [0x01, 0x04]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x0b 0x00 0x03 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_C_PIN_UserMMMM_Set_PIN" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa8 0x00)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 62 (group SecretProtect)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x00 0x1d 0x00 0x00 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Next" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x08)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 63 (group SecretProtect); every object of the table 00 00 00 1D; notes *AC8
          (PreconfRow (EveryObject (halfUID 0x00 0x00 0x00 0x1d)) False
            [ (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            ["*AC8"])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 65 (group LockingInfo)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x01 0x00 0x00 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 67 (group Locking)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x02 0x00 0x00 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Next" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x08)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 68 (group Locking)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x02 0x00 0x00 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Locking_GlobalRange_Get_RangeStartToActiveKey" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xd0 0x00), RefCell "ACE_Anybody_Get_CommonName" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x03)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 69 (group Locking)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x02 0x00 0x03 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Locking_Range1_Get_RangeStartToActiveKey" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xd0 0x01), RefCell "ACE_Anybody_Get_CommonName" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x03)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 70 (group Locking); a template, NNNN
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x01, 0x04]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x02 0x00 0x03 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Locking_RangeNNNN_Get_RangeStartToActiveKey" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xd0 0x00), RefCell "ACE_Anybody_Get_CommonName" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x03)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 71 (group Locking)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x02 0x00 0x00 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Locking_GlblRng_Admins_Set" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xf0 0x00), RefCell "ACE_Locking_GlobalRange_Set_RdLocked" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe0 0x00), RefCell "ACE_Locking_GlobalRange_Set_WrLocked" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe8 0x00), RefCell "ACE_Admins_Set_CommonName" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x04)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 72 (group Locking)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x02 0x00 0x03 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Locking_Admins_RangeStartToLOR" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xf0 0x01), RefCell "ACE_Locking_Range1_Set_RdLocked" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe0 0x01), RefCell "ACE_Locking_Range1_Set_WrLocked" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe8 0x01), RefCell "ACE_Admins_Set_CommonName" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x04)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 73 (group Locking); a template, NNNN
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x01, 0x04]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x02 0x00 0x03 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Locking_Admins_RangeStartToLOR" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xf0 0x01), RefCell "ACE_Locking_RangeNNNN_Set_RdLocked" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe0 0x00), RefCell "ACE_Locking_RangeNNNN_Set_WrLocked" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe8 0x00), RefCell "ACE_Admins_Set_CommonName" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x04)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 75 (group MBRControl)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x03 0x00 0x00 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 76 (group MBRControl)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x03 0x00 0x00 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_MBRControl_Admins_Set" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xf8 0x00), RefCell "ACE_MBRControl_Set_DoneToDOR" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xf8 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 78 (group MBR)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x04 0x00 0x00 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 79 (group MBR)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x04 0x00 0x00 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_Admin" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x02)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 81 (group K_AES_128)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x05 0x00 0x00 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_K_AES_Mode" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xbf 0xff)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 82 (group K_AES_128)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x05 0x00 0x03 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_K_AES_Mode" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xbf 0xff)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 83 (group K_AES_128); a template, NNNN
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x01]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x05 0x00 0x03 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_K_AES_Mode" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xbf 0xff)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 84 (group K_AES_128)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x05 0x00 0x00 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "GenKey" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x10)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_K_AES_128_GlobalRange_GenKey" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb0 0x00)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 85 (group K_AES_128)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x05 0x00 0x03 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "GenKey" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x10)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_K_AES_128_Range1_GenKey" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb0 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 86 (group K_AES_128); a template, NNNN
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x01, 0x04]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x05 0x00 0x03 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "GenKey" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x10)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_K_AES_128_RangeNNNN_GenKey" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb0 0x00)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 88 (group K_AES_256)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x06 0x00 0x00 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_K_AES_Mode" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xbf 0xff)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 89 (group K_AES_256)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x06 0x00 0x03 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_K_AES_Mode" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xbf 0xff)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 90 (group K_AES_256); a template, NNNN
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x01]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x06 0x00 0x03 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_K_AES_Mode" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xbf 0xff)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 91 (group K_AES_256)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x06 0x00 0x00 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "GenKey" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x10)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_K_AES_256_GlobalRange_GenKey" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb8 0x00)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 92 (group K_AES_256)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x06 0x00 0x03 0x00 0x01)))  -- InvokingID
            , (0x02, Given (RefCell "GenKey" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x10)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_K_AES_256_Range1_GenKey" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb8 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 93 (group K_AES_256); a template, NNNN
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x01, 0x04]) False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x08 0x06 0x00 0x03 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "GenKey" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x10)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_K_AES_256_RangeNNNN_GenKey" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb8 0x00)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 95 (group DataStore)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x10 0x01 0x00 0x00 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Get" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x16)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_DataStore_Get_All" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xfc 0x00)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      , Pre  -- row 96 (group DataStore)
          (PreconfRow Plain False
            [ (0x01, Given (UIDCell (uid 0x00 0x00 0x10 0x01 0x00 0x00 0x00 0x00)))  -- InvokingID
            , (0x02, Given (RefCell "Set" (uid 0x00 0x00 0x00 0x06 0x00 0x00 0x00 0x17)))  -- MethodID
            , (0x04, Given (ListCell [RefCell "ACE_DataStore_Set_All" (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xfc 0x01)]))  -- ACL
            , (0x08, Given (ListCell [RefCell "ACE_Anybody" (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)]))  -- GetACLACL
            ]
            [])
          [ (0x00, Unspecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, Unspecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, ReadOnlySpecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified)
          ]
      ]
  }

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 78 / p.77, ord 41, sha256:febdc5264fc6): the ACE table (core-2.01 T177) as the Locking SP holds it
lockingSPACETablePreconfiguration :: PreconfTable
lockingSPACETablePreconfiguration = PreconfTable
  { ptDesc     = aceTableDesc
  , ptDefaults =
      [ (0x01, TextCell "")  -- Name
      , (0x02, TextCell "")  -- CommonName
      , (0x03, ListCell [])  -- BooleanExpr
      , (0x04, ListCell [])  -- Columns
      ]
  , ptRows     =
      [ Pre  -- row 2 ACE_Anybody (group Base ACEs)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01)))  -- UID
            , (0x01, Given (TextCell "ACE_Anybody"))  -- Name
            , (0x03, Given (ListCell [RefCell "Anybody" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x01)]))  -- BooleanExpr
            , (0x04, Given (ListCell []))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 3 ACE_Admin (group Base ACEs)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x02)))  -- UID
            , (0x01, Given (TextCell "ACE_Admin"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell []))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 4 ACE_Anybody_Get_CommonName (group Base ACEs)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x03)))  -- UID
            , (0x01, Given (TextCell "ACE_Anybody_Get_CommonName"))  -- Name
            , (0x03, Given (ListCell [RefCell "Anybody" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x01)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 0, UIntCell 2]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 5 ACE_Admins_Set_CommonName (group Base ACEs)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x04)))  -- UID
            , (0x01, Given (TextCell "ACE_Admins_Set_CommonName"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 2]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 7 ACE_ACE_Get_All (group ACE)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_ACE_Get_All"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell []))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 8 ACE_ACE_Set_BooleanExpression (group ACE)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01)))  -- UID
            , (0x01, Given (TextCell "ACE_ACE_Set_BooleanExpression"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 3]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 10 ACE_Authority_Get_All (group Authority)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x90 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_Authority_Get_All"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell []))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 11 ACE_Authority_Set_Enabled (group Authority)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0x90 0x01)))  -- UID
            , (0x01, Given (TextCell "ACE_Authority_Set_Enabled"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 5]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 12 ACE_User1_Set_CommonName (group Authority)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x04 0x40 0x01)))  -- UID
            , (0x01, Given (TextCell "ACE_User1_Set_CommonName"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 2]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 13 ACE_UserMMMM_Set_CommonName (group Authority); a template, MMMM
          (PreconfRow (Template (Placeholder "MMMM" 2) [0x00, 0x01]) False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x04 0x40 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_UserMMMM_Set_CommonName"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 2]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 15 ACE_C_PIN_Admins_Get_All_NOPIN (group C_PIN)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa0 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_C_PIN_Admins_Get_All_NOPIN"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 0, UIntCell 4, UIntCell 5, UIntCell 6, UIntCell 7]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 16 ACE_C_PIN_Admins_Set_PIN (group C_PIN)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa0 0x01)))  -- UID
            , (0x01, Given (TextCell "ACE_C_PIN_Admins_Set_PIN"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 3]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 17 ACE_C_PIN_User1_Set_PIN (group C_PIN); notes *ACE1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa8 0x01)))  -- UID
            , (0x01, Given (TextCell "ACE_C_PIN_User1_Set_PIN"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02), RefCell "User1" (uid 0x00 0x00 0x00 0x09 0x00 0x03 0x00 0x01), EnumCell "OR" 1]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 3]))  -- Columns
            ]
            ["*ACE1"])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 18 ACE_C_PIN_UserMMMM_Set_PIN (group C_PIN); a template, MMMM; optional; notes *ACE1
          (PreconfRow (Template (Placeholder "MMMM" 2) [0x00, 0x01, 0x03]) True
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa8 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_C_PIN_UserMMMM_Set_PIN"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02), RefCell "UserMMMM" (uid 0x00 0x00 0x00 0x09 0x00 0x03 0x00 0x00), EnumCell "OR" 1]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 3]))  -- Columns
            ]
            ["*ACE1"])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 20 ACE_K_AES_Mode (group K_AES)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xbf 0xff)))  -- UID
            , (0x01, Given (TextCell "ACE_K_AES_Mode"))  -- Name
            , (0x03, Given (ListCell [RefCell "Anybody" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x01)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 4]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 22 ACE_K_AES_128_GlobalRange_GenKey (group K_AES_128)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb0 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_K_AES_128_GlobalRange_GenKey"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell []))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 23 ACE_K_AES_128_Range1_GenKey (group K_AES_128)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb0 0x01)))  -- UID
            , (0x01, Given (TextCell "ACE_K_AES_128_Range1_GenKey"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell []))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 24 ACE_K_AES_128_RangeNNNN_GenKey (group K_AES_128); a template, NNNN; optional
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x00, 0x01]) True
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb0 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_K_AES_128_RangeNNNN_GenKey"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell []))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 26 ACE_K_AES_256_GlobalRange_GenKey (group K_AES_256)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb8 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_K_AES_256_GlobalRange_GenKey"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell []))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 27 ACE_K_AES_256_Range1_GenKey (group K_AES_256)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb8 0x01)))  -- UID
            , (0x01, Given (TextCell "ACE_K_AES_256_Range1_GenKey"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell []))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 28 ACE_K_AES_256_RangeNNNN_GenKey (group K_AES_256); a template, NNNN
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x00, 0x01]) False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb8 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_K_AES_256_RangeNNNN_GenKey"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell []))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 30 ACE_Locking_GlobalRange_Get_RangeStartToActiveKey (group Locking)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xd0 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_Locking_GlobalRange_Get_RangeStartToActiveKey"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 3, UIntCell 4, UIntCell 5, UIntCell 6, UIntCell 7, UIntCell 8, UIntCell 9, UIntCell 10]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 31 ACE_Locking_Range1_Get_RangeStartToActiveKey (group Locking)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xd0 0x01)))  -- UID
            , (0x01, Given (TextCell "ACE_Locking_Range1_Get_RangeStartToActiveKey"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 3, UIntCell 4, UIntCell 5, UIntCell 6, UIntCell 7, UIntCell 8, UIntCell 9, UIntCell 10]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 32 ACE_Locking_RangeNNNN_Get_RangeStartToActiveKey (group Locking); a template, NNNN
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x00, 0x01]) False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xd0 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_Locking_RangeNNNN_Get_RangeStartToActiveKey"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 3, UIntCell 4, UIntCell 5, UIntCell 6, UIntCell 7, UIntCell 8, UIntCell 9, UIntCell 10]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 33 ACE_Locking_GlobalRange_Set_RdLocked (group Locking)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe0 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_Locking_GlobalRange_Set_RdLocked"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 7]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 34 ACE_Locking_Range1_Set_RdLocked (group Locking)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe0 0x01)))  -- UID
            , (0x01, Given (TextCell "ACE_Locking_Range1_Set_RdLocked"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 7]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 35 ACE_Locking_RangeNNNN_Set_RdLocked (group Locking); a template, NNNN
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x00, 0x01]) False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe0 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_Locking_RangeNNNN_Set_RdLocked"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 7]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 36 ACE_Locking_GlobalRange_Set_WrLocked (group Locking)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe8 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_Locking_GlobalRange_Set_WrLocked"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 8]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 37 ACE_Locking_Range1_Set_WrLocked (group Locking)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe8 0x01)))  -- UID
            , (0x01, Given (TextCell "ACE_Locking_Range1_Set_WrLocked"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 8]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 38 ACE_Locking_RangeNNNN_Set_WrLocked (group Locking); a template, NNNN
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x00, 0x01]) False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe8 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_Locking_RangeNNNN_Set_WrLocked"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 8]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 39 ACE_Locking_GlblRng_Admins_Set (group Locking)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xf0 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_Locking_GlblRng_Admins_Set"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 5, UIntCell 6, UIntCell 7, UIntCell 8, UIntCell 9]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 40 ACE_Locking_Admins_RangeStartToLOR (group Locking)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xf0 0x01)))  -- UID
            , (0x01, Given (TextCell "ACE_Locking_Admins_RangeStartToLOR"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 3, UIntCell 4, UIntCell 5, UIntCell 6, UIntCell 7, UIntCell 8, UIntCell 9]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 42 ACE_MBRControl_Admins_Set (group MBRControl)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xf8 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_MBRControl_Admins_Set"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 1, UIntCell 2, UIntCell 3]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 43 ACE_MBRControl_Set_DoneToDOR (group MBRControl)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xf8 0x01)))  -- UID
            , (0x01, Given (TextCell "ACE_MBRControl_Set_DoneToDOR"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell [UIntCell 2, UIntCell 3]))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 45 ACE_DataStore_Get_All (group DataStore)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xfc 0x00)))  -- UID
            , (0x01, Given (TextCell "ACE_DataStore_Get_All"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell []))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      , Pre  -- row 46 ACE_DataStore_Set_All (group DataStore)
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x08 0x00 0x03 0xfc 0x01)))  -- UID
            , (0x01, Given (TextCell "ACE_DataStore_Set_All"))  -- Name
            , (0x03, Given (ListCell [RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)]))  -- BooleanExpr
            , (0x04, Given (ListCell []))  -- Columns
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified)
          ]
      ]
  }

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 78 / p.77, ord 41, sha256:febdc5264fc6), row 2: ACE_Anybody
uACE_Anybody :: UID
uACE_Anybody = uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x01

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 78 / p.77, ord 41, sha256:febdc5264fc6), row 3: ACE_Admin
uACE_Admin :: UID
uACE_Admin = uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x02

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 78 / p.77, ord 41, sha256:febdc5264fc6), row 4: ACE_Anybody_Get_CommonName
uACE_Anybody_Get_CommonName :: UID
uACE_Anybody_Get_CommonName = uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x03

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 78 / p.77, ord 41, sha256:febdc5264fc6), row 5: ACE_Admins_Set_CommonName
uACE_Admins_Set_CommonName :: UID
uACE_Admins_Set_CommonName = uid 0x00 0x00 0x00 0x08 0x00 0x00 0x00 0x04

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 78 / p.77, ord 41, sha256:febdc5264fc6), row 7: ACE_ACE_Get_All
uACE_ACE_Get_All :: UID
uACE_ACE_Get_All = uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x00

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 78 / p.77, ord 41, sha256:febdc5264fc6), row 8: ACE_ACE_Set_BooleanExpression
uACE_ACE_Set_BooleanExpression :: UID
uACE_ACE_Set_BooleanExpression = uid 0x00 0x00 0x00 0x08 0x00 0x03 0x80 0x01

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 79 / p.78, ord 41, sha256:febdc5264fc6), row 10: ACE_Authority_Get_All
uACE_Authority_Get_All :: UID
uACE_Authority_Get_All = uid 0x00 0x00 0x00 0x08 0x00 0x03 0x90 0x00

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 79 / p.78, ord 41, sha256:febdc5264fc6), row 11: ACE_Authority_Set_Enabled
uACE_Authority_Set_Enabled :: UID
uACE_Authority_Set_Enabled = uid 0x00 0x00 0x00 0x08 0x00 0x03 0x90 0x01

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 79 / p.78, ord 41, sha256:febdc5264fc6), row 12: ACE_User1_Set_CommonName
uACE_User1_Set_CommonName :: UID
uACE_User1_Set_CommonName = uid 0x00 0x00 0x00 0x08 0x00 0x04 0x40 0x01

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 79 / p.78, ord 41, sha256:febdc5264fc6), row 15: ACE_C_PIN_Admins_Get_All_NOPIN
uACE_C_PIN_Admins_Get_All_NOPIN :: UID
uACE_C_PIN_Admins_Get_All_NOPIN = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa0 0x00

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 79 / p.78, ord 41, sha256:febdc5264fc6), row 16: ACE_C_PIN_Admins_Set_PIN
uACE_C_PIN_Admins_Set_PIN :: UID
uACE_C_PIN_Admins_Set_PIN = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa0 0x01

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 79 / p.78, ord 41, sha256:febdc5264fc6), row 17: ACE_C_PIN_User1_Set_PIN
uACE_C_PIN_User1_Set_PIN :: UID
uACE_C_PIN_User1_Set_PIN = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xa8 0x01

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 79 / p.78, ord 41, sha256:febdc5264fc6), row 20: ACE_K_AES_Mode
uACE_K_AES_Mode :: UID
uACE_K_AES_Mode = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xbf 0xff

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 79 / p.78, ord 41, sha256:febdc5264fc6), row 22: ACE_K_AES_128_GlobalRange_GenKey
uACE_K_AES_128_GlobalRange_GenKey :: UID
uACE_K_AES_128_GlobalRange_GenKey = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb0 0x00

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 79 / p.78, ord 41, sha256:febdc5264fc6), row 23: ACE_K_AES_128_Range1_GenKey
uACE_K_AES_128_Range1_GenKey :: UID
uACE_K_AES_128_Range1_GenKey = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb0 0x01

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 80 / p.79, ord 41, sha256:febdc5264fc6), row 26: ACE_K_AES_256_GlobalRange_GenKey
uACE_K_AES_256_GlobalRange_GenKey :: UID
uACE_K_AES_256_GlobalRange_GenKey = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb8 0x00

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 80 / p.79, ord 41, sha256:febdc5264fc6), row 27: ACE_K_AES_256_Range1_GenKey
uACE_K_AES_256_Range1_GenKey :: UID
uACE_K_AES_256_Range1_GenKey = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xb8 0x01

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 80 / p.79, ord 41, sha256:febdc5264fc6), row 30: ACE_Locking_GlobalRange_Get_RangeStartToActiveKey
uACE_Locking_GlobalRange_Get_RangeStartToActiveKey :: UID
uACE_Locking_GlobalRange_Get_RangeStartToActiveKey = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xd0 0x00

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 80 / p.79, ord 41, sha256:febdc5264fc6), row 31: ACE_Locking_Range1_Get_RangeStartToActiveKey
uACE_Locking_Range1_Get_RangeStartToActiveKey :: UID
uACE_Locking_Range1_Get_RangeStartToActiveKey = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xd0 0x01

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 81 / p.80, ord 41, sha256:febdc5264fc6), row 33: ACE_Locking_GlobalRange_Set_RdLocked
uACE_Locking_GlobalRange_Set_RdLocked :: UID
uACE_Locking_GlobalRange_Set_RdLocked = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe0 0x00

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 81 / p.80, ord 41, sha256:febdc5264fc6), row 34: ACE_Locking_Range1_Set_RdLocked
uACE_Locking_Range1_Set_RdLocked :: UID
uACE_Locking_Range1_Set_RdLocked = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe0 0x01

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 81 / p.80, ord 41, sha256:febdc5264fc6), row 36: ACE_Locking_GlobalRange_Set_WrLocked
uACE_Locking_GlobalRange_Set_WrLocked :: UID
uACE_Locking_GlobalRange_Set_WrLocked = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe8 0x00

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 81 / p.80, ord 41, sha256:febdc5264fc6), row 37: ACE_Locking_Range1_Set_WrLocked
uACE_Locking_Range1_Set_WrLocked :: UID
uACE_Locking_Range1_Set_WrLocked = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xe8 0x01

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 81 / p.80, ord 41, sha256:febdc5264fc6), row 39: ACE_Locking_GlblRng_Admins_Set
uACE_Locking_GlblRng_Admins_Set :: UID
uACE_Locking_GlblRng_Admins_Set = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xf0 0x00

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 81 / p.80, ord 41, sha256:febdc5264fc6), row 40: ACE_Locking_Admins_RangeStartToLOR
uACE_Locking_Admins_RangeStartToLOR :: UID
uACE_Locking_Admins_RangeStartToLOR = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xf0 0x01

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 81 / p.80, ord 41, sha256:febdc5264fc6), row 42: ACE_MBRControl_Admins_Set
uACE_MBRControl_Admins_Set :: UID
uACE_MBRControl_Admins_Set = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xf8 0x00

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 81 / p.80, ord 41, sha256:febdc5264fc6), row 43: ACE_MBRControl_Set_DoneToDOR
uACE_MBRControl_Set_DoneToDOR :: UID
uACE_MBRControl_Set_DoneToDOR = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xf8 0x01

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 82 / p.81, ord 41, sha256:febdc5264fc6), row 45: ACE_DataStore_Get_All
uACE_DataStore_Get_All :: UID
uACE_DataStore_Get_All = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xfc 0x00

-- | opal-2.30 T39 - Locking SP - ACE Table Preconfiguration (PDF 82 / p.81, ord 41, sha256:febdc5264fc6), row 46: ACE_DataStore_Set_All
uACE_DataStore_Set_All :: UID
uACE_DataStore_Set_All = uid 0x00 0x00 0x00 0x08 0x00 0x03 0xfc 0x01

-- | opal-2.30 T40 - Locking SP - Authority Table Preconfiguration (PDF 82 / p.81, ord 42, sha256:6133cfaf69b0): the Authority table (core-2.01 T178) as the Locking SP holds it
lockingSPAuthorityTablePreconfiguration :: PreconfTable
lockingSPAuthorityTablePreconfiguration = PreconfTable
  { ptDesc     = authorityTableDesc
  , ptDefaults =
      [ (0x01, TextCell "")  -- Name
      , (0x02, TextCell "")  -- CommonName
      , (0x03, EnumCell "False" 0)  -- IsClass
      , (0x04, RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- Class
      , (0x05, EnumCell "False" 0)  -- Enabled
      , (0x06, EnumCell "None" 0)  -- Secure
      , (0x07, EnumCell "None" 0)  -- HashAndSign
      , (0x08, EnumCell "False" 0)  -- PresentCertificate
      , (0x09, EnumCell "None" 0)  -- Operation
      , (0x0A, RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- Credential
      , (0x0B, RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- ResponseSign
      , (0x0C, RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- ResponseExch
      , (0x0D, ListCell [])  -- ClockStart
      , (0x0E, ListCell [])  -- ClockEnd
      , (0x0F, UIntCell 0)  -- Limit
      , (0x10, UIntCell 0)  -- Uses
      , (0x11, EnumCell "None" 0)  -- Log
      , (0x12, RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- LogTo
      ]
  , ptRows     =
      [ Pre  -- row 1 Anybody
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x01)))  -- UID
            , (0x01, Given (TextCell "Anybody"))  -- Name
            , (0x02, Given (TextCell ""))  -- CommonName
            , (0x03, Given (EnumCell "F" 0))  -- IsClass
            , (0x04, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- Class
            , (0x05, Given (EnumCell "T" 1))  -- Enabled
            , (0x06, Given (EnumCell "None" 0))  -- Secure
            , (0x07, Given (EnumCell "None" 0))  -- HashAndSign
            , (0x08, Given (EnumCell "F" 0))  -- PresentCertificate
            , (0x09, Given (EnumCell "None" 0))  -- Operation
            , (0x0A, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- Credential
            , (0x0B, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseSign
            , (0x0C, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseExch
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, ReadOnlySpecified), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified), (0x05, ReadOnlySpecified), (0x06, ReadOnlySpecified), (0x07, ReadOnlySpecified)
          , (0x08, ReadOnlySpecified), (0x09, ReadOnlySpecified), (0x0A, ReadOnlySpecified), (0x0B, ReadOnlySpecified)
          , (0x0C, ReadOnlySpecified), (0x0D, Unspecified), (0x0E, Unspecified), (0x0F, Unspecified)
          , (0x10, Unspecified), (0x11, Unspecified), (0x12, Unspecified)
          ]
      , Pre  -- row 2 Admins
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)))  -- UID
            , (0x01, Given (TextCell "Admins"))  -- Name
            , (0x02, Given (TextCell ""))  -- CommonName
            , (0x03, Given (EnumCell "T" 1))  -- IsClass
            , (0x04, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- Class
            , (0x05, Given (EnumCell "T" 1))  -- Enabled
            , (0x06, Given (EnumCell "None" 0))  -- Secure
            , (0x07, Given (EnumCell "None" 0))  -- HashAndSign
            , (0x08, Given (EnumCell "F" 0))  -- PresentCertificate
            , (0x09, Given (EnumCell "None" 0))  -- Operation
            , (0x0A, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- Credential
            , (0x0B, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseSign
            , (0x0C, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseExch
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, ReadOnlySpecified), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified), (0x05, ReadOnlySpecified), (0x06, ReadOnlySpecified), (0x07, ReadOnlySpecified)
          , (0x08, ReadOnlySpecified), (0x09, ReadOnlySpecified), (0x0A, ReadOnlySpecified), (0x0B, ReadOnlySpecified)
          , (0x0C, ReadOnlySpecified), (0x0D, Unspecified), (0x0E, Unspecified), (0x0F, Unspecified)
          , (0x10, Unspecified), (0x11, Unspecified), (0x12, Unspecified)
          ]
      , Pre  -- row 3 Admin1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 0x01)))  -- UID
            , (0x01, Given (TextCell "Admin1"))  -- Name
            , (0x02, Given (TextCell ""))  -- CommonName
            , (0x03, Given (EnumCell "F" 0))  -- IsClass
            , (0x04, Given (RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)))  -- Class
            , (0x05, Given (EnumCell "T" 1))  -- Enabled
            , (0x06, Given (EnumCell "None" 0))  -- Secure
            , (0x07, Given (EnumCell "None" 0))  -- HashAndSign
            , (0x08, Given (EnumCell "F" 0))  -- PresentCertificate
            , (0x09, Given (EnumCell "Password" 1))  -- Operation
            , (0x0A, Given (RefCell "C_PIN_Admin1" (uid 0x00 0x00 0x00 0x0b 0x00 0x01 0x00 0x01)))  -- Credential
            , (0x0B, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseSign
            , (0x0C, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseExch
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, WriteFixed), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified), (0x05, ReadOnlySpecified), (0x06, ReadOnlySpecified), (0x07, ReadOnlySpecified)
          , (0x08, ReadOnlySpecified), (0x09, ReadOnlySpecified), (0x0A, ReadOnlySpecified), (0x0B, ReadOnlySpecified)
          , (0x0C, ReadOnlySpecified), (0x0D, Unspecified), (0x0E, Unspecified), (0x0F, Unspecified)
          , (0x10, Unspecified), (0x11, Unspecified), (0x12, Unspecified)
          ]
      , Pre  -- row 4 Admin2
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 0x02)))  -- UID
            , (0x01, Given (TextCell "Admin2"))  -- Name
            , (0x02, Given (TextCell ""))  -- CommonName
            , (0x03, Given (EnumCell "F" 0))  -- IsClass
            , (0x04, Given (RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)))  -- Class
            , (0x05, Given (EnumCell "F" 0))  -- Enabled
            , (0x06, Given (EnumCell "None" 0))  -- Secure
            , (0x07, Given (EnumCell "None" 0))  -- HashAndSign
            , (0x08, Given (EnumCell "F" 0))  -- PresentCertificate
            , (0x09, Given (EnumCell "Password" 1))  -- Operation
            , (0x0A, Given (RefCell "C_PIN_Admin2" (uid 0x00 0x00 0x00 0x0b 0x00 0x01 0x00 0x02)))  -- Credential
            , (0x0B, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseSign
            , (0x0C, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseExch
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, WriteFixed), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified), (0x05, WriteFixed), (0x06, ReadOnlySpecified), (0x07, ReadOnlySpecified)
          , (0x08, ReadOnlySpecified), (0x09, ReadOnlySpecified), (0x0A, ReadOnlySpecified), (0x0B, ReadOnlySpecified)
          , (0x0C, ReadOnlySpecified), (0x0D, Unspecified), (0x0E, Unspecified), (0x0F, Unspecified)
          , (0x10, Unspecified), (0x11, Unspecified), (0x12, Unspecified)
          ]
      , Pre  -- row 5 Admin3
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 0x03)))  -- UID
            , (0x01, Given (TextCell "Admin3"))  -- Name
            , (0x02, Given (TextCell ""))  -- CommonName
            , (0x03, Given (EnumCell "F" 0))  -- IsClass
            , (0x04, Given (RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)))  -- Class
            , (0x05, Given (EnumCell "F" 0))  -- Enabled
            , (0x06, Given (EnumCell "None" 0))  -- Secure
            , (0x07, Given (EnumCell "None" 0))  -- HashAndSign
            , (0x08, Given (EnumCell "F" 0))  -- PresentCertificate
            , (0x09, Given (EnumCell "Password" 1))  -- Operation
            , (0x0A, Given (RefCell "C_PIN_Admin3" (uid 0x00 0x00 0x00 0x0b 0x00 0x01 0x00 0x03)))  -- Credential
            , (0x0B, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseSign
            , (0x0C, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseExch
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, WriteFixed), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified), (0x05, WriteFixed), (0x06, ReadOnlySpecified), (0x07, ReadOnlySpecified)
          , (0x08, ReadOnlySpecified), (0x09, ReadOnlySpecified), (0x0A, ReadOnlySpecified), (0x0B, ReadOnlySpecified)
          , (0x0C, ReadOnlySpecified), (0x0D, Unspecified), (0x0E, Unspecified), (0x0F, Unspecified)
          , (0x10, Unspecified), (0x11, Unspecified), (0x12, Unspecified)
          ]
      , Pre  -- row 6 Admin4
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 0x04)))  -- UID
            , (0x01, Given (TextCell "Admin4"))  -- Name
            , (0x02, Given (TextCell ""))  -- CommonName
            , (0x03, Given (EnumCell "F" 0))  -- IsClass
            , (0x04, Given (RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)))  -- Class
            , (0x05, Given (EnumCell "F" 0))  -- Enabled
            , (0x06, Given (EnumCell "None" 0))  -- Secure
            , (0x07, Given (EnumCell "None" 0))  -- HashAndSign
            , (0x08, Given (EnumCell "F" 0))  -- PresentCertificate
            , (0x09, Given (EnumCell "Password" 1))  -- Operation
            , (0x0A, Given (RefCell "C_PIN_Admin4" (uid 0x00 0x00 0x00 0x0b 0x00 0x01 0x00 0x04)))  -- Credential
            , (0x0B, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseSign
            , (0x0C, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseExch
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, WriteFixed), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified), (0x05, WriteFixed), (0x06, ReadOnlySpecified), (0x07, ReadOnlySpecified)
          , (0x08, ReadOnlySpecified), (0x09, ReadOnlySpecified), (0x0A, ReadOnlySpecified), (0x0B, ReadOnlySpecified)
          , (0x0C, ReadOnlySpecified), (0x0D, Unspecified), (0x0E, Unspecified), (0x0F, Unspecified)
          , (0x10, Unspecified), (0x11, Unspecified), (0x12, Unspecified)
          ]
      , Pre  -- row 7 AdminXXXX; a template, XXXX; optional; notes 1
          (PreconfRow (Template (Placeholder "XXXX" 2) [0x00, 0x01]) True
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 0x00)))  -- UID
            , (0x01, Given (TextCell "AdminXXXX"))  -- Name
            , (0x02, Given (TextCell ""))  -- CommonName
            , (0x03, Given (EnumCell "F" 0))  -- IsClass
            , (0x04, Given (RefCell "Admins" (uid 0x00 0x00 0x00 0x09 0x00 0x00 0x00 0x02)))  -- Class
            , (0x05, Given (EnumCell "F" 0))  -- Enabled
            ]
            ["1"])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, WriteFixed), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified), (0x05, WriteFixed), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified), (0x0F, Unspecified)
          , (0x10, Unspecified), (0x11, Unspecified), (0x12, Unspecified)
          ]
      , Pre  -- row 8 Users
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x03 0x00 0x00)))  -- UID
            , (0x01, Given (TextCell "Users"))  -- Name
            , (0x02, Given (TextCell ""))  -- CommonName
            , (0x03, Given (EnumCell "T" 1))  -- IsClass
            , (0x04, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- Class
            , (0x05, Given (EnumCell "T" 1))  -- Enabled
            , (0x06, Given (EnumCell "None" 0))  -- Secure
            , (0x07, Given (EnumCell "None" 0))  -- HashAndSign
            , (0x08, Given (EnumCell "F" 0))  -- PresentCertificate
            , (0x09, Given (EnumCell "None" 0))  -- Operation
            , (0x0A, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- Credential
            , (0x0B, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseSign
            , (0x0C, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseExch
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, ReadOnlySpecified), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified), (0x05, ReadOnlySpecified), (0x06, ReadOnlySpecified), (0x07, ReadOnlySpecified)
          , (0x08, ReadOnlySpecified), (0x09, ReadOnlySpecified), (0x0A, ReadOnlySpecified), (0x0B, ReadOnlySpecified)
          , (0x0C, ReadOnlySpecified), (0x0D, Unspecified), (0x0E, Unspecified), (0x0F, Unspecified)
          , (0x10, Unspecified), (0x11, Unspecified), (0x12, Unspecified)
          ]
      , Pre  -- row 9 User1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x03 0x00 0x01)))  -- UID
            , (0x01, Given (TextCell "User1"))  -- Name
            , (0x02, Given (TextCell ""))  -- CommonName
            , (0x03, Given (EnumCell "F" 0))  -- IsClass
            , (0x04, Given (RefCell "Users" (uid 0x00 0x00 0x00 0x09 0x00 0x03 0x00 0x00)))  -- Class
            , (0x05, Given (EnumCell "F" 0))  -- Enabled
            , (0x06, Given (EnumCell "None" 0))  -- Secure
            , (0x07, Given (EnumCell "None" 0))  -- HashAndSign
            , (0x08, Given (EnumCell "F" 0))  -- PresentCertificate
            , (0x09, Given (EnumCell "Password" 1))  -- Operation
            , (0x0A, Given (RefCell "C_PIN_User1" (uid 0x00 0x00 0x00 0x0b 0x00 0x03 0x00 0x01)))  -- Credential
            , (0x0B, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseSign
            , (0x0C, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseExch
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, WritePersonalizable), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified), (0x05, WriteFixed), (0x06, ReadOnlySpecified), (0x07, ReadOnlySpecified)
          , (0x08, ReadOnlySpecified), (0x09, ReadOnlySpecified), (0x0A, ReadOnlySpecified), (0x0B, ReadOnlySpecified)
          , (0x0C, ReadOnlySpecified), (0x0D, Unspecified), (0x0E, Unspecified), (0x0F, Unspecified)
          , (0x10, Unspecified), (0x11, Unspecified), (0x12, Unspecified)
          ]
      , Pre  -- row 10 UserMMMM; a template, MMMM; optional; notes 2
          (PreconfRow (Template (Placeholder "MMMM" 2) [0x00, 0x01, 0x0A]) True
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x09 0x00 0x03 0x00 0x00)))  -- UID
            , (0x01, Given (TextCell "UserMMMM"))  -- Name
            , (0x02, Given (TextCell ""))  -- CommonName
            , (0x03, Given (EnumCell "F" 0))  -- IsClass
            , (0x04, Given (RefCell "Users" (uid 0x00 0x00 0x00 0x09 0x00 0x03 0x00 0x00)))  -- Class
            , (0x05, Given (EnumCell "F" 0))  -- Enabled
            , (0x06, Given (EnumCell "None" 0))  -- Secure
            , (0x07, Given (EnumCell "None" 0))  -- HashAndSign
            , (0x08, Given (EnumCell "F" 0))  -- PresentCertificate
            , (0x09, Given (EnumCell "Password" 1))  -- Operation
            , (0x0A, Given (RefCell "C_PIN_UserMMMM" (uid 0x00 0x00 0x00 0x0b 0x00 0x03 0x00 0x00)))  -- Credential
            , (0x0B, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseSign
            , (0x0C, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- ResponseExch
            ]
            ["2"])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, WritePersonalizable), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified), (0x05, WriteFixed), (0x06, ReadOnlySpecified), (0x07, ReadOnlySpecified)
          , (0x08, ReadOnlySpecified), (0x09, ReadOnlySpecified), (0x0A, ReadOnlySpecified), (0x0B, ReadOnlySpecified)
          , (0x0C, ReadOnlySpecified), (0x0D, Unspecified), (0x0E, Unspecified), (0x0F, Unspecified)
          , (0x10, Unspecified), (0x11, Unspecified), (0x12, Unspecified)
          ]
      ]
  }

-- | opal-2.30 T40 - Locking SP - Authority Table Preconfiguration (PDF 82 / p.81, ord 42, sha256:6133cfaf69b0), row 3: Admin1
uAuthority_Admin1 :: UID
uAuthority_Admin1 = uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 0x01

-- | opal-2.30 T40 - Locking SP - Authority Table Preconfiguration (PDF 82 / p.81, ord 42, sha256:6133cfaf69b0), row 4: Admin2
uAuthority_Admin2 :: UID
uAuthority_Admin2 = uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 0x02

-- | opal-2.30 T40 - Locking SP - Authority Table Preconfiguration (PDF 83 / p.82, ord 42, sha256:6133cfaf69b0), row 5: Admin3
uAuthority_Admin3 :: UID
uAuthority_Admin3 = uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 0x03

-- | opal-2.30 T40 - Locking SP - Authority Table Preconfiguration (PDF 83 / p.82, ord 42, sha256:6133cfaf69b0), row 6: Admin4
uAuthority_Admin4 :: UID
uAuthority_Admin4 = uid 0x00 0x00 0x00 0x09 0x00 0x01 0x00 0x04

-- | opal-2.30 T40 - Locking SP - Authority Table Preconfiguration (PDF 83 / p.82, ord 42, sha256:6133cfaf69b0), row 8: Users
uAuthority_Users :: UID
uAuthority_Users = uid 0x00 0x00 0x00 0x09 0x00 0x03 0x00 0x00

-- | opal-2.30 T40 - Locking SP - Authority Table Preconfiguration (PDF 83 / p.82, ord 42, sha256:6133cfaf69b0), row 9: User1
uAuthority_User1 :: UID
uAuthority_User1 = uid 0x00 0x00 0x00 0x09 0x00 0x03 0x00 0x01

-- | opal-2.30 T41 - Locking SP - C_PIN Table Preconfiguration (PDF 84 / p.83, ord 43, sha256:aa7f5dee6d00): the C_PIN table (core-2.01 T181) as the Locking SP holds it
lockingSPCPINTablePreconfiguration :: PreconfTable
lockingSPCPINTablePreconfiguration = PreconfTable
  { ptDesc     = c_PINTableDesc
  , ptDefaults =
      [ (0x01, TextCell "")  -- Name
      , (0x02, TextCell "")  -- CommonName
      , (0x03, BytesCell "")  -- PIN
      , (0x04, RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- CharSet
      , (0x05, UIntCell 0)  -- TryLimit
      , (0x06, UIntCell 0)  -- Tries
      , (0x07, EnumCell "False" 0)  -- Persistence
      ]
  , ptRows     =
      [ Pre  -- row 1 C_PIN_Admin1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x0b 0x00 0x01 0x00 0x01)))  -- UID
            , (0x01, Given (TextCell "C_PIN_Admin1"))  -- Name
            , (0x03, NoteMarked 1 (VendorUnique "SID or MSID"))  -- PIN; STAND-IN: hand-edited (NoteMarked)
            , (0x04, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- CharSet
            , (0x05, Given (UIntCell 0))  -- TryLimit
            , (0x06, Given (UIntCell 0))  -- Tries
            , (0x07, Given (EnumCell "FALSE" 0))  -- Persistence
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified), (0x05, ReadOnlyVU), (0x06, ReadOnlyVU), (0x07, ReadOnlySpecified)
          ]
      , Pre  -- row 2 C_PIN_Admin2
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x0b 0x00 0x01 0x00 0x02)))  -- UID
            , (0x01, Given (TextCell "C_PIN_Admin2"))  -- Name
            , (0x03, Given (BytesCell ""))  -- PIN
            , (0x04, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- CharSet
            , (0x05, Given (UIntCell 0))  -- TryLimit
            , (0x06, Given (UIntCell 0))  -- Tries
            , (0x07, Given (EnumCell "FALSE" 0))  -- Persistence
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified), (0x05, ReadOnlyVU), (0x06, ReadOnlyVU), (0x07, ReadOnlySpecified)
          ]
      , Pre  -- row 3 C_PIN_Admin3
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x0b 0x00 0x01 0x00 0x03)))  -- UID
            , (0x01, Given (TextCell "C_PIN_Admin3"))  -- Name
            , (0x03, Given (BytesCell ""))  -- PIN
            , (0x04, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- CharSet
            , (0x05, Given (UIntCell 0))  -- TryLimit
            , (0x06, Given (UIntCell 0))  -- Tries
            , (0x07, Given (EnumCell "FALSE" 0))  -- Persistence
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified), (0x05, ReadOnlyVU), (0x06, ReadOnlyVU), (0x07, ReadOnlySpecified)
          ]
      , Pre  -- row 4 C_PIN_Admin4
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x0b 0x00 0x01 0x00 0x04)))  -- UID
            , (0x01, Given (TextCell "C_PIN_Admin4"))  -- Name
            , (0x03, Given (BytesCell ""))  -- PIN
            , (0x04, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- CharSet
            , (0x05, Given (UIntCell 0))  -- TryLimit
            , (0x06, Given (UIntCell 0))  -- Tries
            , (0x07, Given (EnumCell "FALSE" 0))  -- Persistence
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified), (0x05, ReadOnlyVU), (0x06, ReadOnlyVU), (0x07, ReadOnlySpecified)
          ]
      , Pre  -- row 5 C_PIN_AdminXXXX; a template, XXXX; optional
          (PreconfRow (Template (Placeholder "XXXX" 2) [0x00, 0x01]) True
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x0b 0x00 0x01 0x00 0x00)))  -- UID
            , (0x01, Given (TextCell "C_PIN_AdminXXXX"))  -- Name
            , (0x03, Given (BytesCell ""))  -- PIN
            , (0x04, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- CharSet
            , (0x05, Given (UIntCell 0))  -- TryLimit
            , (0x06, Given (UIntCell 0))  -- Tries
            , (0x07, Given (EnumCell "FALSE" 0))  -- Persistence
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlySpecified), (0x05, ReadOnlyVU), (0x06, ReadOnlyVU), (0x07, ReadOnlySpecified)
          ]
      , Pre  -- row 6 C_PIN_User1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x0b 0x00 0x03 0x00 0x01)))  -- UID
            , (0x01, Given (TextCell "C_PIN_User1"))  -- Name
            , (0x03, Given (BytesCell ""))  -- PIN
            , (0x04, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- CharSet
            , (0x05, Given (UIntCell 0))  -- TryLimit
            , (0x06, Given (UIntCell 0))  -- Tries
            , (0x07, Given (EnumCell "FALSE" 0))  -- Persistence
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WritePersonalizable)
          , (0x04, ReadOnlySpecified), (0x05, ReadOnlyVU), (0x06, ReadOnlyVU), (0x07, ReadOnlySpecified)
          ]
      , Pre  -- row 7 C_PIN_UserMMMM; a template, MMMM; optional
          (PreconfRow (Template (Placeholder "MMMM" 2) [0x00, 0x01]) True
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x0b 0x00 0x03 0x00 0x00)))  -- UID
            , (0x01, Given (TextCell "C_PIN_UserMMMM"))  -- Name
            , (0x03, Given (BytesCell ""))  -- PIN
            , (0x04, Given (RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)))  -- CharSet
            , (0x05, Given (UIntCell 0))  -- TryLimit
            , (0x06, Given (UIntCell 0))  -- Tries
            , (0x07, Given (EnumCell "FALSE" 0))  -- Persistence
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WritePersonalizable)
          , (0x04, ReadOnlySpecified), (0x05, ReadOnlyVU), (0x06, ReadOnlyVU), (0x07, ReadOnlySpecified)
          ]
      ]
  }

-- | opal-2.30 T41 - Locking SP - C_PIN Table Preconfiguration (PDF 84 / p.83, ord 43, sha256:aa7f5dee6d00), row 1: C_PIN_Admin1
uC_PIN_Admin1 :: UID
uC_PIN_Admin1 = uid 0x00 0x00 0x00 0x0b 0x00 0x01 0x00 0x01

-- | opal-2.30 T41 - Locking SP - C_PIN Table Preconfiguration (PDF 84 / p.83, ord 43, sha256:aa7f5dee6d00), row 2: C_PIN_Admin2
uC_PIN_Admin2 :: UID
uC_PIN_Admin2 = uid 0x00 0x00 0x00 0x0b 0x00 0x01 0x00 0x02

-- | opal-2.30 T41 - Locking SP - C_PIN Table Preconfiguration (PDF 84 / p.83, ord 43, sha256:aa7f5dee6d00), row 3: C_PIN_Admin3
uC_PIN_Admin3 :: UID
uC_PIN_Admin3 = uid 0x00 0x00 0x00 0x0b 0x00 0x01 0x00 0x03

-- | opal-2.30 T41 - Locking SP - C_PIN Table Preconfiguration (PDF 84 / p.83, ord 43, sha256:aa7f5dee6d00), row 4: C_PIN_Admin4
uC_PIN_Admin4 :: UID
uC_PIN_Admin4 = uid 0x00 0x00 0x00 0x0b 0x00 0x01 0x00 0x04

-- | opal-2.30 T41 - Locking SP - C_PIN Table Preconfiguration (PDF 84 / p.83, ord 43, sha256:aa7f5dee6d00), row 6: C_PIN_User1
uC_PIN_User1 :: UID
uC_PIN_User1 = uid 0x00 0x00 0x00 0x0b 0x00 0x03 0x00 0x01

-- | opal-2.30 T42 - Locking SP - SecretProtect Table Preconfiguration (PDF 84 / p.83, ord 44, sha256:9429543fe16a): the SecretProtect table (core-2.01 T176) as the Locking SP holds it
lockingSPSecretProtectTablePreconfiguration :: PreconfTable
lockingSPSecretProtectTablePreconfiguration = PreconfTable
  { ptDesc     = secretProtectTableDesc
  , ptDefaults =
      [ (0x01, RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- Table
      , (0x02, UIntCell 0)  -- ColumnNumber
      ]
  , ptRows     =
      [ Pre  -- row 1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x1d 0x00 0x00 0x00 0x1d)))  -- UID
            , (0x01, Given (RefCell "K_AES_128" (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x08 0x05)))  -- Table
            , (0x02, Given (UIntCell 0x03))  -- ColumnNumber
            , (0x03, VendorUnique "VU")  -- ProtectMechanisms
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, ReadOnlyVU)
          ]
      , Pre  -- row 2
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x00 0x1d 0x00 0x00 0x00 0x1e)))  -- UID
            , (0x01, Given (RefCell "K_AES_256" (uid 0x00 0x00 0x00 0x01 0x00 0x00 0x08 0x06)))  -- Table
            , (0x02, Given (UIntCell 0x03))  -- ColumnNumber
            , (0x03, VendorUnique "VU")  -- ProtectMechanisms
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, ReadOnlySpecified), (0x02, ReadOnlySpecified), (0x03, ReadOnlyVU)
          ]
      ]
  }

-- | opal-2.30 T44 - Locking SP - LockingInfo Table Preconfiguration (PDF 85 / p.84, ord 46, sha256:d48eecf9ed64): the LockingInfo table (core-2.01 T225, with opal-2.30's columns) as the Locking SP holds it
lockingSPLockingInfoTablePreconfiguration :: PreconfTable
lockingSPLockingInfoTablePreconfiguration = PreconfTable
  { ptDesc     = lockingInfoTableDesc
  , ptDefaults =
      [ (0x01, TextCell "")  -- Name
      , (0x02, UIntCell 0)  -- Version
      , (0x03, EnumCell "None" 0)  -- EncryptSupport
      , (0x04, UIntCell 0)  -- MaxRanges
      , (0x05, UIntCell 0)  -- MaxReEncryptions
      , (0x06, EnumCell "None" 0)  -- KeysAvailableCfg
      , (0x07, EnumCell "False" 0)  -- AlignmentRequired
      , (0x08, UIntCell 0)  -- LogicalBlockSize
      , (0x09, UIntCell 0)  -- AlignmentGranularity
      , (0x0A, UIntCell 0)  -- LowestAlignedLBA
      ]
  , ptRows     =
      [ Pre  -- row 1 
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x08 0x01 0x00 0x00 0x00 0x01)))  -- UID
            , (0x03, Given (EnumCell "Media Encryption" 1))  -- EncryptSupport
            , (0x04, NoteMarked 1 (Given (UIntCell 8)))  -- MaxRanges; STAND-IN: hand-edited (NoteMarked)
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified), (0x05, Unspecified), (0x06, Unspecified), (0x07, Unspecified)
          , (0x08, Unspecified), (0x09, Unspecified), (0x0A, Unspecified)
          ]
      ]
  }

-- | opal-2.30 T45 - Locking SP - Locking Table Preconfiguration (PDF 86 / p.85, ord 47, sha256:2d261424e7a1): the Locking table (core-2.01 T226) as the Locking SP holds it
lockingSPLockingTablePreconfiguration :: PreconfTable
lockingSPLockingTablePreconfiguration = PreconfTable
  { ptDesc     = lockingTableDesc
  , ptDefaults =
      [ (0x01, TextCell "")  -- Name
      , (0x02, TextCell "")  -- CommonName
      , (0x03, UIntCell 0)  -- RangeStart
      , (0x04, UIntCell 0)  -- RangeLength
      , (0x05, EnumCell "False" 0)  -- ReadLockEnabled
      , (0x06, EnumCell "False" 0)  -- WriteLockEnabled
      , (0x07, EnumCell "False" 0)  -- ReadLocked
      , (0x08, EnumCell "False" 0)  -- WriteLocked
      , (0x0A, RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- ActiveKey
      , (0x0B, RefCell "Null" (uid 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00))  -- NextKey
      , (0x0C, EnumCell "Idle" 1)  -- ReEncryptState
      , (0x0D, EnumCell "1" 1)  -- ReEncryptRequest
      , (0x0E, EnumCell "Wait for AdvKey_Req" 0)  -- AdvKeyMode
      , (0x0F, EnumCell "No verify" 0)  -- VerifyMode
      , (0x11, UIntCell 0)  -- LastReEncryptLBA
      , (0x12, EnumCell "Success" 0)  -- LastReEncStat
      ]
  , ptRows     =
      [ Pre  -- row 1 Locking_GlobalRange; notes *LT1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x08 0x02 0x00 0x00 0x00 0x01)))  -- UID
            , (0x01, Given (TextCell "Locking_GlobalRange"))  -- Name
            , (0x02, Given (TextCell ""))  -- CommonName
            , (0x03, Given (UIntCell 0))  -- RangeStart
            , (0x04, Given (UIntCell 0))  -- RangeLength
            , (0x05, Given (EnumCell "F" 0))  -- ReadLockEnabled
            , (0x06, Given (EnumCell "F" 0))  -- WriteLockEnabled
            , (0x07, Given (EnumCell "F" 0))  -- ReadLocked
            , (0x08, Given (EnumCell "F" 0))  -- WriteLocked
            , (0x0A, OneOf [Given (RefCell "K_AES_128_GlobalRange_Key" (uid 0x00 0x00 0x08 0x05 0x00 0x00 0x00 0x01)), Given (RefCell "K_AES_256_GlobalRange_Key" (uid 0x00 0x00 0x08 0x06 0x00 0x00 0x00 0x01))])  -- ActiveKey; STAND-IN: hand-edited (OneOf)
            ]
            ["*LT1"])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, WriteFixed), (0x03, ReadOnlySpecified)
          , (0x04, ReadOnlySpecified), (0x05, WriteFixed), (0x06, WriteFixed), (0x07, WritePersonalizable)
          , (0x08, WritePersonalizable), (0x09, Unspecified), (0x0A, ReadOnlySpecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified), (0x0F, Unspecified)
          , (0x10, Unspecified), (0x11, Unspecified), (0x12, Unspecified), (0x13, Unspecified)
          ]
      , Pre  -- row 2 Locking_Range1; notes *LT1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x08 0x02 0x00 0x03 0x00 0x01)))  -- UID
            , (0x01, Given (TextCell "Locking_Range1"))  -- Name
            , (0x02, Given (TextCell ""))  -- CommonName
            , (0x03, Given (UIntCell 0))  -- RangeStart
            , (0x04, Given (UIntCell 0))  -- RangeLength
            , (0x05, Given (EnumCell "F" 0))  -- ReadLockEnabled
            , (0x06, Given (EnumCell "F" 0))  -- WriteLockEnabled
            , (0x07, Given (EnumCell "F" 0))  -- ReadLocked
            , (0x08, Given (EnumCell "F" 0))  -- WriteLocked
            , (0x0A, OneOf [Given (RefCell "K_AES_128_Range1_Key" (uid 0x00 0x00 0x08 0x05 0x00 0x03 0x00 0x01)), Given (RefCell "K_AES_256_Range1_Key" (uid 0x00 0x00 0x08 0x06 0x00 0x03 0x00 0x01))])  -- ActiveKey; STAND-IN: hand-edited (OneOf)
            ]
            ["*LT1"])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, WriteFixed), (0x03, WriteFixed)
          , (0x04, WriteFixed), (0x05, WriteFixed), (0x06, WriteFixed), (0x07, WritePersonalizable)
          , (0x08, WritePersonalizable), (0x09, Unspecified), (0x0A, ReadOnlySpecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified), (0x0F, Unspecified)
          , (0x10, Unspecified), (0x11, Unspecified), (0x12, Unspecified), (0x13, Unspecified)
          ]
      , Pre  -- row 3 Locking_RangeNNNN; a template, NNNN; notes *LT1
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x00, 0x01, 0x0A]) False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x08 0x02 0x00 0x03 0x00 0x00)))  -- UID
            , (0x01, Given (TextCell "Locking_RangeNNNN"))  -- Name
            , (0x02, Given (TextCell ""))  -- CommonName
            , (0x03, Given (UIntCell 0))  -- RangeStart
            , (0x04, Given (UIntCell 0))  -- RangeLength
            , (0x05, Given (EnumCell "F" 0))  -- ReadLockEnabled
            , (0x06, Given (EnumCell "F" 0))  -- WriteLockEnabled
            , (0x07, Given (EnumCell "F" 0))  -- ReadLocked
            , (0x08, Given (EnumCell "F" 0))  -- WriteLocked
            , (0x0A, OneOf [Given (RefCell "K_AES_128_RangeNNNN_Key" (uid 0x00 0x00 0x08 0x05 0x00 0x03 0x00 0x00)), Given (RefCell "K_AES_256_RangeNNNN_Key" (uid 0x00 0x00 0x08 0x06 0x00 0x03 0x00 0x00))])  -- ActiveKey; STAND-IN: hand-edited (OneOf)
            ]
            ["*LT1"])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, WriteFixed), (0x03, WriteFixed)
          , (0x04, WriteFixed), (0x05, WriteFixed), (0x06, WriteFixed), (0x07, WritePersonalizable)
          , (0x08, WritePersonalizable), (0x09, Unspecified), (0x0A, ReadOnlySpecified), (0x0B, Unspecified)
          , (0x0C, Unspecified), (0x0D, Unspecified), (0x0E, Unspecified), (0x0F, Unspecified)
          , (0x10, Unspecified), (0x11, Unspecified), (0x12, Unspecified), (0x13, Unspecified)
          ]
      ]
  }

-- | opal-2.30 T45 - Locking SP - Locking Table Preconfiguration (PDF 86 / p.85, ord 47, sha256:2d261424e7a1), row 2: Locking_Range1
uLocking_Range1 :: UID
uLocking_Range1 = uid 0x00 0x00 0x08 0x02 0x00 0x03 0x00 0x01

-- | opal-2.30 T46 - Locking SP - MBRControl Table Preconfiguration (PDF 88 / p.87, ord 50, sha256:b4b2152439fa): the MBRControl table (core-2.01 T229) as the Locking SP holds it
lockingSPMBRControlTablePreconfiguration :: PreconfTable
lockingSPMBRControlTablePreconfiguration = PreconfTable
  { ptDesc     = mbrControlTableDesc
  , ptDefaults =
      [ (0x01, EnumCell "False" 0)  -- Enable
      , (0x02, EnumCell "False" 0)  -- Done
      ]
  , ptRows     =
      [ Pre  -- row 1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x08 0x03 0x00 0x00 0x00 0x01)))  -- UID
            , (0x01, Given (EnumCell "False" 0))  -- Enable
            , (0x02, Given (EnumCell "False" 0))  -- Done
            ]
            [])
          [ (0x00, ReadOnlySpecified), (0x01, WriteFixed), (0x02, WritePersonalizable), (0x03, Unspecified)
          ]
      ]
  }

-- | opal-2.30 T47 - Locking SP - K_AES_128 Table Preconfiguration (PDF 89 / p.88, ord 51, sha256:5f92cf2192e8): the K_AES_128 table (core-2.01 T227) as the Locking SP holds it
lockingSPKAES128TablePreconfiguration :: PreconfTable
lockingSPKAES128TablePreconfiguration = PreconfTable
  { ptDesc     = k_AES_128TableDesc
  , ptDefaults =
      [ (0x01, TextCell "")  -- Name
      , (0x02, TextCell "")  -- CommonName
      , (0x04, EnumCell "ECB" 0)  -- Mode
      ]
  , ptRows     =
      [ Pre  -- row 1 K_AES_128_GlobalRange_Key; notes *K1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x08 0x05 0x00 0x00 0x00 0x01)))  -- UID
            , (0x01, Given (TextCell "K_AES_128_GlobalRange_Key"))  -- Name
            , (0x03, VendorUnique "VU")  -- Key
            , (0x04, VendorUnique "VU")  -- Mode
            ]
            ["*K1"])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlyVU)
          ]
      , Pre  -- row 2 K_AES_128_Range1_Key; notes *K1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x08 0x05 0x00 0x03 0x00 0x01)))  -- UID
            , (0x01, Given (TextCell "K_AES_128_Range1_Key"))  -- Name
            , (0x03, VendorUnique "VU")  -- Key
            , (0x04, VendorUnique "VU")  -- Mode
            ]
            ["*K1"])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlyVU)
          ]
      , Pre  -- row 3 K_AES_128_RangeNNNN_Key; a template, NNNN; optional; notes *K1
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x00, 0x01]) True
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x08 0x05 0x00 0x03 0x00 0x00)))  -- UID
            , (0x01, Given (TextCell "K_AES_128_RangeNNNN_Key"))  -- Name
            , (0x03, VendorUnique "VU")  -- Key
            , (0x04, VendorUnique "VU")  -- Mode
            ]
            ["*K1"])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlyVU)
          ]
      ]
  }

-- | opal-2.30 T47 - Locking SP - K_AES_128 Table Preconfiguration (PDF 89 / p.88, ord 51, sha256:5f92cf2192e8), row 1: K_AES_128_GlobalRange_Key
uK_AES_128_GlobalRange_Key :: UID
uK_AES_128_GlobalRange_Key = uid 0x00 0x00 0x08 0x05 0x00 0x00 0x00 0x01

-- | opal-2.30 T47 - Locking SP - K_AES_128 Table Preconfiguration (PDF 89 / p.88, ord 51, sha256:5f92cf2192e8), row 2: K_AES_128_Range1_Key
uK_AES_128_Range1_Key :: UID
uK_AES_128_Range1_Key = uid 0x00 0x00 0x08 0x05 0x00 0x03 0x00 0x01

-- | opal-2.30 T48 - Locking SP - K_AES_256 Table Preconfiguration (PDF 89 / p.88, ord 52, sha256:039e4af03cd8): the K_AES_256 table (core-2.01 T228) as the Locking SP holds it
lockingSPKAES256TablePreconfiguration :: PreconfTable
lockingSPKAES256TablePreconfiguration = PreconfTable
  { ptDesc     = k_AES_256TableDesc
  , ptDefaults =
      [ (0x01, TextCell "")  -- Name
      , (0x02, TextCell "")  -- CommonName
      , (0x04, EnumCell "ECB" 0)  -- Mode
      ]
  , ptRows     =
      [ Pre  -- row 1 K_AES_256_GlobalRange_Key; notes *K1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x08 0x06 0x00 0x00 0x00 0x01)))  -- UID
            , (0x01, Given (TextCell "K_AES_256_GlobalRange_Key"))  -- Name
            , (0x03, VendorUnique "VU")  -- Key
            , (0x04, VendorUnique "VU")  -- Mode
            ]
            ["*K1"])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlyVU)
          ]
      , Pre  -- row 2 K_AES_256_Range1_Key; notes *K1
          (PreconfRow Plain False
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x08 0x06 0x00 0x03 0x00 0x01)))  -- UID
            , (0x01, Given (TextCell "K_AES_256_Range1_Key"))  -- Name
            , (0x03, VendorUnique "VU")  -- Key
            , (0x04, VendorUnique "VU")  -- Mode
            ]
            ["*K1"])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlyVU)
          ]
      , Pre  -- row 3 K_AES_256_RangeNNNN_Key; a template, NNNN; optional; notes *K1
          (PreconfRow (Template (Placeholder "NNNN" 2) [0x00, 0x01]) True
            [ (0x00, Given (UIDCell (uid 0x00 0x00 0x08 0x06 0x00 0x03 0x00 0x00)))  -- UID
            , (0x01, Given (TextCell "K_AES_256_RangeNNNN_Key"))  -- Name
            , (0x03, VendorUnique "VU")  -- Key
            , (0x04, VendorUnique "VU")  -- Mode
            ]
            ["*K1"])
          [ (0x00, ReadOnlySpecified), (0x01, Unspecified), (0x02, Unspecified), (0x03, WriteFixed)
          , (0x04, ReadOnlyVU)
          ]
      ]
  }

-- | opal-2.30 T48 - Locking SP - K_AES_256 Table Preconfiguration (PDF 89 / p.88, ord 52, sha256:039e4af03cd8), row 1: K_AES_256_GlobalRange_Key
uK_AES_256_GlobalRange_Key :: UID
uK_AES_256_GlobalRange_Key = uid 0x00 0x00 0x08 0x06 0x00 0x00 0x00 0x01

-- | opal-2.30 T48 - Locking SP - K_AES_256 Table Preconfiguration (PDF 89 / p.88, ord 52, sha256:039e4af03cd8), row 2: K_AES_256_Range1_Key
uK_AES_256_Range1_Key :: UID
uK_AES_256_Range1_Key = uid 0x00 0x00 0x08 0x06 0x00 0x03 0x00 0x01

-- | The Locking SP's preconfiguration tables, in table order: opal-2.30 T34, T35, T36, T37, T38, T39, T40, T41, T42, T44, T45, T46, T47, T48
lockingSP :: [PreconfTable]
lockingSP =
  [ lockingSPSPInfoTablePreconfiguration
  , lockingSPSPTemplatesTablePreconfiguration
  , lockingSPTableTablePreconfiguration
  , lockingSPMethodIDTablePreconfiguration
  , lockingSPAccessControlTablePreconfiguration
  , lockingSPACETablePreconfiguration
  , lockingSPAuthorityTablePreconfiguration
  , lockingSPCPINTablePreconfiguration
  , lockingSPSecretProtectTablePreconfiguration
  , lockingSPLockingInfoTablePreconfiguration
  , lockingSPLockingTablePreconfiguration
  , lockingSPMBRControlTablePreconfiguration
  , lockingSPKAES128TablePreconfiguration
  , lockingSPKAES256TablePreconfiguration
  ]

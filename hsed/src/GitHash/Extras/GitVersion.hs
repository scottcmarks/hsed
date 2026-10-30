{-|
Module      : GitHash.Extras.GitVersion
Description : Git info appended to a version string
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Git info appended to a version string
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}

module GitHash.Extras.GitVersion (gitBranch, gitVersion, plainVersion)
where

import           Data.Version               (Version(..), showVersion)
import           GitHash                    (giBranch, giDirty,
                                             giHash, tGitInfoCwdTry)
import           Language.Haskell.TH        (Q,Exp)
import qualified Language.Haskell.TH.Syntax as TH(lift)
import           Prelude                    (($), (<>), either)




-- | Generate a string like @Version 1.2)@.
--
-- @$(plainVersion …)@ @::@ 'String'
plainVersion :: Version -> Q Exp
plainVersion version = [|"Version " ++ $(TH.lift $ showVersion version)|]


-- | Generate a string like @feature/foo 12c45b7 (dirty)@.
--
-- @$(gitVersion …)@ @::@ 'String'
gitBranch :: Q Exp
gitBranch = either noGitInfo withGitInfo $$tGitInfoCwdTry
  where
    -- The tree may be an export or an sdist: say so instead of refusing to build.
    noGitInfo s    = [| "no git info: " <> $(TH.lift s) |]
    withGitInfo gi = [| concat [ giBranch gi, " ", take 7 $ giHash gi
                               , if giDirty gi then " (dirty)" else ""
                               ]
                      |]


-- | Generate a string like @Version 1.2 [feature/foo 12e45b7 (dirty)]@.
--
-- @$(gitVersion …)@ @::@ 'String'
gitVersion :: Version -> Q Exp
gitVersion version = [|concat [ $(plainVersion version), " [", $gitBranch, "]"]|]

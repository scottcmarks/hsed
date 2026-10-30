-- | The `bindings` test component: compiling the committed, generated bindings
-- is the test (docs/spec-as-source.md section 4.2); running it just reports.
module Main (main) where

import qualified Hsed.Bindings.Core as Core
import qualified Hsed.Bindings.Opal as Opal

main :: IO ()
main = putStrLn ("bindings: " <> show (length Core.bindings) <> " of core-2.01 and "
                 <> show (length Opal.bindings) <> " of opal-2.30 compiled")

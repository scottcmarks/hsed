-- | The `bindings` test component: compiling the committed, generated bindings
-- is the test (docs/spec-as-source.md section 4.2); running it just reports.
module Main (main) where

import           Hsed.Bindings.Core (bindings)

main :: IO ()
main = putStrLn ("bindings: " <> show (length bindings) <> " compiled")

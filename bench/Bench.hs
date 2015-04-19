{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
-- | $ cabal configure --enable-benchmarks && cabal build && cabal bench
import           Commands.Backends.OSX    (showActions)
import           Commands.Core
import           Commands.Plugins.Example hiding (main)

import           Control.Lens
import           Criterion.Main

import qualified Data.List                as List


-- loudly fails on parse error
benchInterpret c x s = bench message $ nf (either (error.show) showActions . interpret c x) s
 where
 message = List.intercalate " "
  ["{", "interpret", show s, "with", showLHS (c^.comGrammar.gramRule.ruleLHS), "at", show x, "}"]

main = defaultMain
 [ bgroup "interpret"
  [ benchInterpret root "emacs" "replace par round grave camel lit with async break break action with blank" -- ≤1ms!
  ]
 ]

{-# LANGUAGE NamedFieldPuns, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
-- | $ cabal configure --enable-benchmarks && cabal build && cabal bench
import           Commands.Core
import           Commands.Plugins.Example hiding (main)
-- import           Commands.Backends.OSX    (showActions)

import           Control.Lens
import           Criterion.Main
import           Criterion.Measurement    (measure)
import           Criterion.Types          (Measured (..))

import           Data.List.NonEmpty       ()

import qualified Data.List                as List


-- loudly fails on parse error
-- Show constraint to avoid NFData constraint
benchParse :: Show a => (forall z. Rule (EarleyProduction z LHS) r LHS String a) -> String -> Benchmark
benchParse rule s = bench message $ nf (either (error.show) show . runRuleParser rule) (words s)
 where
 message = List.intercalate " " ["{", "parse", show s, "with", showLHS (rule^.ruleLHS), "}"]

-- main = defaultMainWith defaultConfig
--  [ bgroup "parse"
--   [ benchParse (root^.comRule) "replace par round grave camel lit with async break break action with blank" --
--   ]
--  ]

-- main = time (nf (either (error.show) show . runRuleParser rule) (words s))

main = do
 (Measured{measTime},_) <- measure (nf (either (error.show) show . runRuleParser (root^.comRule)) (words "replace par round grave camel lit with async break break action with blank")) 1
 print $ measTime

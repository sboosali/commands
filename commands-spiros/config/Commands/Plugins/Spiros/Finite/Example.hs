{-# LANGUAGE NoMonadComprehensions, LambdaCase, LiberalTypeSynonyms, RankNTypes, ExistentialQuantification, ViewPatterns  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Plugins.Spiros.Finite.Example where
import Commands.Plugins.Spiros.Extra
--import Commands.Plugins.Spiros.Edit ()
import Commands.Plugins.Spiros.Finite.Grammar (finite)
import Commands.Plugins.Spiros.Root (click)
import Commands.RHS.Finite

import           Commands.Mixins.DNS13OSX9
import      Commands.Frontends.Dragon13

import Control.Lens (itraverse_)

import qualified Data.Text.Lazy as T
import Control.Monad (join)


printClick = do

 let (gClick, pClick) = test_observeParserAndGrammar click
 putStrLn""
 print$ pClick "click"
 print$ pClick "double click"
 print$ pClick "middle click"
 print$ pClick "triple right click"
 print$ pClick "quadruple outer click"
 putStrLn""
 putStrLn$ gClick

printFinite = do
 putStrLn""
 putStrLn"FINITE"
 putStrLn""
 putStrLn"(raw) Grammar ="
 print finiteProof
 putStrLn""
 putStrLn"(pretty) Grammar ="
 printFiniteGrammar finiteProof
 putStrLn""
 putStrLn"(dns) Grammar ="
 putStrLn $ displaySerializedGrammar finiteDNS


 -- case finiteSentences of
 --   Nothing  -> do
 --     putStrLn "(infinite)"
 --   Just wss -> do
 --     putStrLn "(finite)"
 --     putStrLn""
 --     putStrLn"(All) Sentences ="
 --     putStrLn"(Unique) Sentences ="
 --     printSentences wss
 --     putStrLn"Length ="
 --     printLength wss
 --     putStrLn""

 putStrLn""

 where

 printFiniteGrammar = maybe (putStrLn "ø") putStrLn . fmap displayFiniteGrammar . join

 printLength = maybe (putStrLn "ø") print . fmap (length :: [a] -> Int)

 printSentences wss = do
   itraverse_ (\i ws -> putStr (show i ++ " ") >> printSentence ws) wss

finiteSentences = finite &
--  (rhsEnumerateSentencesSimply >>> (fmap.fmap.fmap) T.unpack)
  (rhsEnumerateSentences (const ignoreIsFiniteGrammar) >>> (fmap.fmap.fmap) T.unpack)

finiteProof = finite & isFiniteDNSEarleyGrammar

finiteDNS = unsafeDNSGrammar defaultDnsOptimizationSettings finite

{-

printAllSentences someSentences  = do
 (traverse_.traverse_)  printSentences someSentences

 where
 printSentences sentences = do
  putStrLn""
  traverse_ printSentence $ sentences

myFiniteSentences = theFiniteGrammars

 where
 theFiniteGrammars
  = (fmap.fmap.fmap.fmap) T.unpack
  . fmap (\(SomeDNSEarleyRHS r) -> rhsEnumerateSentencesSimply r)
  $ allFiniteRhs
--TODO (getRhsName r, isFiniteDNSEarleyGrammar r)

allFiniteRhs :: [SomeDNSEarleyRHS]
allFiniteRhs =
 [ SomeDNSEarleyRHS move
 , SomeDNSEarleyRHS edit
 -- , SomeDNSEarleyRHS
 ]

 printSentences sentences = do
  putStrLn""
  traverse_ printSentence $ sentences

-}

{-

FiniteAlternatives
 [ FiniteAlternatives
   [ FiniteSequence
      (FiniteSequence
(FiniteAlternatives [FiniteTerminal "sell",FiniteTerminal "copy",FiniteTerminal "cut",FiniteTerminal "del",FiniteTerminal "trans",FiniteTerminal "google"])
        (FiniteOptional (FiniteAlternatives [FiniteTerminal "whole",FiniteTerminal "back",FiniteTerminal "fuss"])))
      (FiniteOptional (FiniteAlternatives [FiniteTerminal "that",FiniteTerminal "char",FiniteTerminal "word",FiniteTerminal "toke",FiniteTerminal "group",FiniteTerminal "line",FiniteTerminal "wreck",FiniteTerminal "block",FiniteTerminal "page",FiniteTerminal "screen",FiniteTerminal "all",FiniteTerminal "def",FiniteTerminal "fun",FiniteTerminal "ref",FiniteTerminal "struct"]))
   , FiniteSequence (FiniteSequence (FiniteOptional (FiniteAlternatives [FiniteTerminal "sell",FiniteTerminal "copy",FiniteTerminal "cut",FiniteTerminal "del",FiniteTerminal "trans",FiniteTerminal "google"])) (FiniteAlternatives [FiniteTerminal "whole",FiniteTerminal "back",FiniteTerminal "fuss"])) (FiniteAlternatives [FiniteTerminal "that",FiniteTerminal "char",FiniteTerminal "word",FiniteTerminal "toke",FiniteTerminal "group",FiniteTerminal "line",FiniteTerminal "wreck",FiniteTerminal "block",FiniteTerminal "page",FiniteTerminal "screen",FiniteTerminal "all",FiniteTerminal "def",FiniteTerminal "fun",FiniteTerminal "ref",FiniteTerminal "struct"])],FiniteAlternatives [FiniteSequence (FiniteAlternatives [FiniteTerminal "up",FiniteTerminal "down",FiniteTerminal "left",FiniteTerminal "right",FiniteTerminal "in",FiniteTerminal "out"]) (FiniteAlternatives [FiniteTerminal "that",FiniteTerminal "char",FiniteTerminal "word",FiniteTerminal "toke",FiniteTerminal "group",FiniteTerminal "line",FiniteTerminal "wreck",FiniteTerminal "block",FiniteTerminal "page",FiniteTerminal "screen",FiniteTerminal "all",FiniteTerminal "def",FiniteTerminal "fun",FiniteTerminal "ref",FiniteTerminal "struct"]),FiniteSequence (FiniteAlternatives [FiniteTerminal "beg",FiniteTerminal "end"]) (FiniteAlternatives [FiniteTerminal "that",FiniteTerminal "char",FiniteTerminal "word",FiniteTerminal "toke",FiniteTerminal "group",FiniteTerminal "line",FiniteTerminal "wreck",FiniteTerminal "block",FiniteTerminal "page",FiniteTerminal "screen",FiniteTerminal "all",FiniteTerminal "def",FiniteTerminal "fun",FiniteTerminal "ref",FiniteTerminal "struct"])]]

((("sell" | "copy" | "cut" | "del" | "trans" | "google") (("whole" | "back" | "fuss"))? (("that" | "char" | "word" | "toke" | "group" | "line" | "wreck" | "block" | "page" | "screen" | "all" | "def" | "fun" | "ref" | "struct"))? | (("sell" | "copy" | "cut" | "del" | "trans" | "google"))? ("whole" | "back" | "fuss") ("that" | "char" | "word" | "toke" | "group" | "line" | "wreck" | "block" | "page" | "screen" | "all" | "def" | "fun" | "ref" | "struct")) | (("up" | "down" | "left" | "right" | "in" | "out") ("that" | "char" | "word" | "toke" | "group" | "line" | "wreck" | "block" | "page" | "screen" | "all" | "def" | "fun" | "ref" | "struct") | ("beg" | "end") ("that" | "char" | "word" | "toke" | "group" | "line" | "wreck" | "block" | "page" | "screen" | "all" | "def" | "fun" | "ref" | "struct")))

(edit | move)
=
((action slice? region? | action? slice region) | (direction region | endpoint region))

action slice? region?
=
action
action        region
action slice
action slice region

action? slice region
=
       slice region
action slice region

dedup { action slice region }

(action slice? region? | action? slice region | direction region | endpoint region)
=
action
action        region
action slice
action slice region
       slice region
   direction region
    endpoint region

|action|    = 6
|slice|     = 3
|region|    = 15
|direction| = 6
|endpoint|  = 2

6 + 6*15 + 6*3 + 6*3*15 + 3*15 + 6*15 + 2*15
=
549

- prove acyclic
- de-associate into Alternative's
- dedup
- inline NonTerminal's, bottom-up
-

-}

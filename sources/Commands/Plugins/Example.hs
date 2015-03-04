{-# LANGUAGE DeriveDataTypeable, ExtendedDefaultRules, LambdaCase #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, RankNTypes       #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TupleSections  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind -fno-warn-orphans -fno-warn-unused-imports -fno-warn-type-defaults #-}
module Commands.Plugins.Example where
import           Commands.Etc                      ()
import           Commands.Frontends.Dragon13
import           Commands.Frontends.Dragon13.Text
import           Commands.Frontends.Dragon13.Types
import           Commands.Grammar
import           Commands.Grammar.Types
import           Commands.Parse
import           Commands.Parse.Types
import           Commands.Render
import           Control.Alternative.Free.Tree

import           Control.Applicative
import           Control.Applicative.Permutation
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad                     (void, (<=<), (>=>))
import           Control.Parallel
import           Data.Bitraversable
import           Data.Either                       (either)
import           Data.Foldable                     (Foldable (..), asum,
                                                    traverse_)
import           Data.List.NonEmpty                (fromList)
import qualified Data.Text.Lazy.IO                 as T
import           Data.Typeable
import           Language.Python.Common.AST        (Expr (Dictionary, Strings))
import           Language.Python.Version2.Parser   (parseExpr, parseModule)
import           Prelude                           hiding (foldr)
import           System.Timeout                    (timeout)
import           Text.PrettyPrint.Leijen.Text      hiding (empty, int, (<$>),
                                                    (<>))


-- import qualified Data.Text.Lazy                    as T
-- import Control.Lens (alongside,Prism',Traversal',Lens)
-- import Control.Exception.Lens (handling, _IOException, AsIOException)
-- import System.IO.Error.Lens (description, location)
-- -- import System.IO.Error.Lens (errorType,_UserError,description)
-- import           Data.Monoid                       ((<>))
-- import           Data.Either.Validation            (Validation (..))
-- import Commands.Parsec
-- import Control.Applicative.Permutation
-- import Data.List                       (intercalate)
-- import Data.Monoid                     ((<>))
-- import Data.Traversable (traverse)


data Root
 = ReplaceWith Dictation Dictation
 | Undo
 | Repeat Positive Root
 deriving (Show,Eq)

root :: Rule Root
root = 'root <=> empty
 <|> ReplaceWith <$> (terminal "replace" *> project dictation) <*> (terminal "with" *> project dictation)
 <|> Undo        <$ terminal "no way"
 <|> Undo        <$ terminal "no"
 <|> Repeat      <$> project positive <*> project root

newtype Positive = Positive Int deriving (Show,Eq)
positive :: Rule Positive
positive = 'positive
 <=> Positive # (asum . fmap int) [1..9]

newtype Dictation = Dictation String deriving (Show,Eq)
dictation :: Rule Dictation
dictation = 'dictation
 <=> Dictation # terminals ["this","that","bike","here","there"]


-- dictation = grammar "<dgndictation>" $ \context ->
--  Dictation <$> anyWord `manyUntil` context

-- -- | context-sensitive grammars (like 'dictation') work (?) with 'atom'
-- data Directions = Directions Dictation Dictation Dictation deriving (Show,Eq)
-- directions :: Rule Directions
-- directions = terminal "directions" *> (runPerms $ Directions
--  <$> atom (terminal "from" *> dictation)
--  <*> atom (terminal "to"   *> dictation)
--  <*> atom (terminal "by"   *> dictation))


type Place = Dictation
place = dictation

data Transport = Foot | Bike | Bus | Car deriving (Show,Ord,Eq,Enum,Typeable)
transport = defaultRule

-- | context-free grammars (like from 'twig' or 'anyWord') can use 'maybeAtom'
data Directions_ = Directions_ (Maybe Place) (Maybe Place) (Maybe Transport) deriving (Show,Eq)
directions :: Rule Directions_
directions = 'directions <=> terminal "directions" *> (runPerms $ Directions_
 <$> maybeAtom (terminal "from" *> project place)
 <*> maybeAtom (terminal "to"   *> project place)
 <*> maybeAtom (terminal "by"   *> project transport))


exampleDirections =
 [ "directions from here to there  by bike"
 , "directions from here by bike   to there"
 , "directions to there  from here by bike"
 , "directions to there  by bike   from here"
 , "directions by bike   from here to there"
 , "directions by bike   to there  from here"
 ]

-- goodDirections  = Directions  (Dictation ["here"])  (Dictation ["there"])  (Dictation ["bike"])
-- goodDirections_ = Directions_ (Just (Place "here")) (Just (Place "there")) (Just Bike)

-- powerset :: [a] -> [[a]]
-- powerset [] = [[]]
-- powerset (x:xs) = xss <> fmap (x:) xss
--  where xss = powerset xs

-- inputDirections_  = fmap (intercalate " ") . fmap ("directions":) . powerset $ ["by bike","to there","from here"]
-- outputDirections_ =
--  [ Directions_ Nothing               Nothing                Nothing
--  , Directions_ (Just (Place "here")) Nothing                Nothing
--  , Directions_ Nothing               (Just (Place "there")) Nothing
--  , Directions_ (Just (Place "here")) (Just (Place "there")) Nothing
--  , Directions_ Nothing               Nothing                (Just Bike)
--  , Directions_ (Just (Place "here")) Nothing                (Just Bike)
--  , Directions_ Nothing               (Just (Place "there")) (Just Bike)
--  , Directions_ (Just (Place "here")) (Just (Place "there")) (Just Bike)
--  ]

grammar = DNSGrammar export [command, subcommand, flag] :: DNSGrammar Text Text

export = DNSProduction (DNSRule "export") $ fromList
 [ DNSSequence $ fromList
   [ DNSNonTerminal (DNSList "command")
   , DNSNonTerminal (DNSRule "subcommand")
   , DNSOptional (DNSMultiple (DNSNonTerminal (DNSList "flag")))
   ]
 , DNSTerminal (DNSToken "ls")
 ]

command = DNSVocabulary (DNSList "command")
 [ DNSToken "git"
 , DNSToken "rm"
 ]

subcommand = DNSProduction (DNSRule "subcommand") $ fromList
 [ DNSTerminal (DNSToken "status")
 , DNSNonTerminal (DNSBuiltin DGNDictation)
 ]

flag = DNSVocabulary (DNSList "flag")
 [ DNSPronounced "-f" "force"
 , DNSPronounced "-r" "recursive"
 , DNSPronounced "-a" "all"
 , DNSPronounced "-i" "interactive"
 ]

-- | traverse with monoidal error collecting
badGrammar = DNSGrammar (DNSProduction (DNSRule "bad root") $ fromList [DNSTerminal (DNSToken "'")]) [] :: DNSGrammar Text Text


isPythonDict :: String -> Bool
isPythonDict s = case parseExpr s "" of
 Right (Dictionary {}, _) -> True
 _ -> False

isPythonString :: String -> Bool
isPythonString s = case parseExpr s "" of
 Right (Strings {}, _) -> True
 _ -> False

isPythonModule :: String -> Bool
isPythonModule s = case parseModule s "" of
 Right {} -> True
 _ -> False

oneSecond :: Int
oneSecond = round (1e6 :: Double)

attemptAsynchronously action = do
 (timeout oneSecond action) `withAsync` (waitCatch >=> \case
   Left error     -> print error
   Right Nothing  -> putStrLn "."
   Right (Just _) -> return ()
  )


attempt = attemptAsynchronously

attemptParse p s = attempt (print =<< (p `parses` s))

attemptSerialize g = attempt $ either print T.putStrLn (serialize (render g))


main = do
 -- let Right escaped = escapeDNSGrammar grammar
 -- let serializedRules = vsep ["'''", serializeRules escaped, "'''"]
 -- let serializedLists = serializeVocabularies (dnsProductions escaped)
 -- let serializedGrammar = vsep $ punctuate "\n" [ "_commands_rules_ =" <+> serializedRules, "_commands_lists_ =" <+> serializedLists]
 -- print serializedGrammar

 -- putStrLn ""
 -- putStr "words: "
 -- print $ getWords grammar
 -- putStr "names: "
 -- print $ getNames grammar

 -- putStrLn ""
 -- putStrLn . take 100 . show $ parseExpr (show serializedRules) ""
 -- putStrLn ""
 -- putStrLn . take 100 . show $ parseExpr (show serializedLists) ""
 -- putStrLn ""
 -- putStrLn . take 100 . show $ parseModule (show serializedGrammar) ""

 -- putStrLn ""
 -- print $ (isPythonString . show) serializedRules
 -- print $ (isPythonDict . show) serializedLists
 -- print $ (isPythonModule . show) serializedGrammar

 -- putStrLn ""
 -- _ <- bitraverse print T.putStrLn $ serialize grammar
 -- putStrLn ""
 -- _ <- bitraverse print T.putStrLn $ serialize badGrammar
 -- putStrLn ""

 -- print escaped

 putStrLn ""
 attempt . print . render $ root
 putStrLn ""
 attemptSerialize root

 putStrLn ""
 handleParse positive "9"
 handleParse dictation "that"
 attemptParse root "no"
 attemptParse root "no way"
 attemptParse root "replace this with that"
 attemptParse root "1 no"
 attemptParse root "1 1 no"

 putStrLn ""
 traverse_ (attemptParse directions) exampleDirections

 putStrLn ""

{-# LANGUAGE OverloadedStrings, PatternSynonyms, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables                            #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Plugins.Example where
import           Commands.Etc                      ()
import           Commands.Frontends.Dragon13
import           Commands.Frontends.Dragon13.Text
import           Commands.Frontends.Dragon13.Types
import qualified Data.Text.Lazy                    as T
-- import Commands.Parse
-- import Commands.Parse.Types
-- import Commands.Parsec
-- import Control.Applicative
-- import Control.Applicative.Permutation
-- import Data.Foldable                   (asum)
-- import Data.List                       (intercalate)
-- import Data.Monoid                     ((<>))
-- import Data.Traversable (traverse)


-- data Command
--  = ReplaceWith Dictation Dictation
--  | Undo
--  | Repeat Positive Command
--  deriving (Show,Eq)
-- command :: Grammar Command
-- command
--  = ReplaceWith  <$> (terminal "replace" *> dictation) <*> (terminal "with" *> dictation)
--  <|> Undo         <$  terminal "undo"
--  <|> Repeat       <$> positive <*> command

-- data Test = Test Dictation Command deriving (Show,Eq)
-- test = Test <$> dictation <*> command
-- test' = Test <$> contextualize (string "replace") dictation <*> command

-- newtype Positive = Positive Int deriving (Show,Eq)
-- positive :: Grammar Positive
-- positive = Positive <$> (asum . map int) [1..9]

-- newtype Dictation = Dictation [String] deriving (Show,Eq)
-- dictation :: Grammar Dictation
-- dictation = grammar "<dgndictation>" $ \context ->
--  Dictation <$> anyWord `manyUntil` context

-- -- | context-sensitive grammars (like 'dictation') work (?) with 'atom'
-- data Directions = Directions Dictation Dictation Dictation deriving (Show,Eq)
-- directions :: Grammar Directions
-- directions = terminal "directions" *> (runPerms $ Directions
--  <$> atom (terminal "from" *> dictation)
--  <*> atom (terminal "to"   *> dictation)
--  <*> atom (terminal "by"   *> dictation))


-- data Place = Place String deriving (Show,Eq)
-- place :: Grammar Place
-- place = Place <$> freely "<Place>" anyWord

-- data Transport = Foot | Bike | Bus | Car deriving (Show,Eq,Enum)
-- transport :: Grammar Transport
-- transport = twig

-- -- | context-free grammars (like from 'twig' or 'anyWord') can use 'maybeAtom'
-- data DirectionsF = DirectionsF (Maybe Place) (Maybe Place) (Maybe Transport) deriving (Show,Eq)
-- directions' :: Grammar DirectionsF
-- directions' = terminal "directions" *> (runPerms $ DirectionsF
--  <$> maybeAtom (terminal "from" *> place)
--  <*> maybeAtom (terminal "to"   *> place)
--  <*> maybeAtom (terminal "by"   *> transport))


-- exampleDirections =
--  [ "directions from here to there  by bike"
--  , "directions from here by bike   to there"
--  , "directions to there  from here by bike"
--  , "directions to there  by bike   from here"
--  , "directions by bike   from here to there"
--  , "directions by bike   to there  from here"
--  ]

-- goodDirections  = Directions  (Dictation ["here"])  (Dictation ["there"])  (Dictation ["bike"])
-- goodDirectionsF = DirectionsF (Just (Place "here")) (Just (Place "there")) (Just Bike)

-- powerset :: [a] -> [[a]]
-- powerset [] = [[]]
-- powerset (x:xs) = xss <> map (x:) xss
--  where xss = powerset xs

-- inputDirectionsF  = map (intercalate " ") . map ("directions":) . powerset $ ["by bike","to there","from here"]
-- outputDirectionsF =
--  [ DirectionsF Nothing               Nothing                Nothing
--  , DirectionsF (Just (Place "here")) Nothing                Nothing
--  , DirectionsF Nothing               (Just (Place "there")) Nothing
--  , DirectionsF (Just (Place "here")) (Just (Place "there")) Nothing
--  , DirectionsF Nothing               Nothing                (Just Bike)
--  , DirectionsF (Just (Place "here")) Nothing                (Just Bike)
--  , DirectionsF Nothing               (Just (Place "there")) (Just Bike)
--  , DirectionsF (Just (Place "here")) (Just (Place "there")) (Just Bike)
--  ]

grammar = DNSGrammar command [subcommand, flag] :: DNSGrammar Text Text

command = DNSProduction (DNSRule "command")
 [ DNSSequence
   [ DNSTerminal (DNSToken "git")
   , DNSNonTerminal (DNSRule "subcommand")
   , DNSOptional (DNSMultiple (DNSNonTerminal (DNSList "flag")))
   ]
 , DNSTerminal (DNSToken "ls")
 ]

subcommand = DNSProduction (DNSRule "subcommand")
 [ DNSTerminal (DNSToken "status")
 , DNSNonTerminal (DNSBuiltin DGNDictation)
 ]

flag = DNSVocabulary (DNSList "flag")
 [ DNSPronounced "-f" "force"
 , DNSPronounced "-r" "recursive"
 , DNSPronounced "-a" "all"
 , DNSPronounced "-i" "interactive"
 ]


main = do
 putStrLn ""
 escaped <- escapeDNSGrammar grammar
 putStrLn . T.unpack . serialize' 50 $ escaped

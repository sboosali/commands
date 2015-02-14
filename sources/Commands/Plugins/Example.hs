{-# LANGUAGE OverloadedStrings, PatternSynonyms, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables                            #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Plugins.Example where
import           Commands.Etc                      ()
import           Commands.Frontends.Dragon13
import           Commands.Frontends.Dragon13.Text
import           Commands.Frontends.Dragon13.Types
-- import qualified Data.Text.Lazy                    as T
import qualified Data.Text.Lazy.IO                 as T
import           Text.PrettyPrint.Leijen.Text      hiding ((<>))
-- import Control.Lens (alongside,Prism',Traversal',Lens)
-- import Control.Exception.Lens (handling, _IOException, AsIOException)
-- import System.IO.Error.Lens (description, location)
-- -- import System.IO.Error.Lens (errorType,_UserError,description)
-- import Control.Monad.Catch (SomeException)
-- import           Data.Monoid                       ((<>))
import           Data.Either.Validation            (Validation (..))


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

grammar = DNSGrammar root [command, subcommand, flag] :: DNSGrammar Text Text

root = DNSProduction (DNSRule "root")
 [ DNSSequence
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


-- | traverse with monoidal error collecting
badGrammar = DNSGrammar (DNSProduction (DNSRule "bad root") [DNSTerminal (DNSToken "'")]) [] :: DNSGrammar Text Text


w = 10
main = do
 let Success escaped = escapeDNSGrammar grammar
 putStrLn ""
 T.putStrLn . displaySerialization w $ "gramSpec =" <+> serializeGrammar escaped
 putStrLn ""
 T.putStrLn . displaySerialization w $ "_commands_lists =" <+> serializeVocabularies (dnsProductions escaped)

 putStrLn ""
 putStr "words: "
 print $ getWords grammar
 putStr "names: "
 print $ getNames grammar

 -- handling ERROR with HANDLER run ACTION
 let Failure errors = escapeDNSGrammar badGrammar
 print errors

 -- handling
 --  (_IOException . description)
 --  (\(dl) -> putStrLn $ "caught+thrown: {" <> dl <> "}")
 --  (escapeDNSGrammar badGrammar >> putStrLn "uncaught/unthrown")

--
-- _IOException :: Prism' IOException   IOException
-- _IOException :: Prism' SomeException IOException
-- description  :: Lens'  IOException   String
-- location     :: Lens'  IOException   String
-- errorType    :: Lens'  IOException   IOErrorType
-- _UserError   :: Prism' IOErrorType   ()
--
-- alongside :: LensLike (Context a b) s t a b -> LensLike (Context a' b') s' t' a' b' -> Lens (s, s') (t, t') (a, a') (b, b')
-- alongside :: Lens' s a -> Lens' s' a' -> Lens' (s, s') (a,a')
-- alongside :: Lens' IOException String -> Lens' IOException String -> Lens' (IOException,IOException) (String,String)
--
--  :: Lens' s a -> Lens' s a' -> Lens' s (a,a')
--  :: Lens' IOException String -> Lens' IOException String -> Lens' IOException (String,String)
--
--  :: Lens' s a -> Lens' (s,s) a
--   just clone the whole. for exception handling, maybe there is a more efficient alternative
--  :: Lens s (s,s) a a
--   think composing lenses, not applying functions!
--
-- infixr . 9
-- a . (b . c)
--

-- forked :: Lens (s,s) s a a
-- forked = undefined

  -- (_IOException . forked . alongside description location)
  -- (\(d,l) -> putStrLn $ "caught+thrown: {" <> d <> "} at " <> l)

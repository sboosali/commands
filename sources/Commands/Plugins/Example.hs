{-# LANGUAGE DeriveDataTypeable, ExtendedDefaultRules, LambdaCase #-}
{-# LANGUAGE NamedFieldPuns, PatternSynonyms, RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TupleSections  #-}
{-# LANGUAGE ViewPatterns                                         #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind -fno-warn-orphans -fno-warn-unused-imports -fno-warn-type-defaults #-}
module Commands.Plugins.Example where
import           Commands.Command
import           Commands.Command.Combinator
import           Commands.Command.Sugar
import           Commands.Command.Types             ()
import           Commands.Etc                       ()
import           Commands.Frontends.Dragon13
import           Commands.Frontends.Dragon13.Render
import           Commands.Frontends.Dragon13.Text
import           Commands.Frontends.Dragon13.Types
import           Commands.Grammar
import           Commands.Grammar.Types
import           Commands.Graph
import           Commands.Parse
import           Commands.Parse.Types
import           Commands.Parsec
import           Control.Alternative.Free.Tree

import           Control.Applicative
import           Control.Applicative.Permutation
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Lens                       hiding (( # ), (&))
import           Control.Monad                      (void, (<=<), (>=>))
import           Control.Monad.Catch                (catches)
import           Control.Parallel
import           Data.Bitraversable
import           Data.Either                        (either)
import           Data.Foldable                      (Foldable (..), asum,
                                                     traverse_)
import           Data.List.NonEmpty                 (NonEmpty (..), fromList)
import qualified Data.Text.Lazy.IO                  as T
import           Data.Typeable
import           Prelude                            hiding (foldr)
import           System.Timeout                     (timeout)
import           Text.PrettyPrint.Leijen.Text       hiding (brackets, empty,
                                                     int, (<$>), (<>))


-- import qualified Data.Text.Lazy                    as T
-- import Control.Lens (alongside,Prism',Traversal',Lens)
-- import Control.Exception.Lens (handling, _IOException, AsIOException)
-- import System.IO.Error.Lens (description, location)
-- -- import System.IO.Error.Lens (errorType,_UserError,description)
-- import           Data.Monoid                       ((<>))
-- import           Data.Either.Validation            (Validation (..))
-- import Control.Applicative.Permutation
-- import Data.List                       (intercalate)
-- import Data.Monoid                     ((<>))
-- import Data.Traversable (traverse)




data Root
 = Repeat Positive Root
 | Edit Action Region
 | Undo
 | ReplaceWith Dictation Dictation
 | Click_ Click
 | Dictated Dictation
 -- Roots [Root]
 deriving (Show,Eq)

root :: Command Root
root = 'root <=> empty
 <|> Repeat      # positive & root
 <|> ReplaceWith # "replace" & dictation & "with" & dictation
 <|> Undo        # "no"         -- order matters..
 <|> Undo        # "no way"     -- .. the superstring "no way" should come before the substring "no" (unlike this example)
 <|> Dictated    # "say" & dictation
 <|> Click_      # click
 <|> Edit        # action    & region
 -- <|> Roots       # (multipleC root)



data Action = Copy | Delete | Next deriving (Show,Eq,Enum,Typeable)
action = enumCommand

data Region = Char | Word | Line deriving (Show,Eq,Enum,Typeable)
region = enumCommand

-- Action = Pick | Go |
-- Region = Selection |
-- Direction = Whole | Backwards | Forwards



type Key = Char
type Keyword = Word
type Words = Dictation
type Separator = String

-- data Phrase
--  deriving (Show)

-- Escaped    "lit" Keyword Phrase        -- ^ "freezes" Phrase into a Word, isomorphic to @['Phrase']@
-- Quoted     "quote" Words "unquote" Phrase  -- ^ "freezes" Phrase into Words
-- Verbatim   "say" Words              -- ^ "freezes" Phrase into Words

-- -- |  will tokens (i.e. Char) work wrt parsing/recognizing? sure, with optional whitespace before/after
-- Pressed "press" [Key] Phrase -- this isn't a Phrase.. it's a Command
-- Spelled "spell" [Char] Phrase  -- ^ manually instantiate NonEmpty, higher-kinded types not in grammar yet, isomorphic to @['Phrase']@
-- Letter  Char Phrase  -- ^ isomorphic to @['Phrase']@
-- Cap     "cap" Char Phrase -- ^ isomorphic to @['Phrase']@

-- -- |
-- -- whitespace or punctuation. un-parsable. Unless I prioritize it?
-- -- no left recursion in parsec, causes bottom. Perform this static check On grammar.
-- -- unless I consume a Word, and then recur? [Separated   Word Phrase Separator Phrase ]
-- -- maybe I can encode the grouping in the right-recursive "modifying" nodes: [Case   Casing Phrase (Maybe Separator)] or both [Case   Casing Phrase] and [CaseSep   Casing Phrase Separator]
-- Separated  Phrase Separator Phrase  --e.g. Space Phrase "space" Phrase
-- --
-- Case     Casing   Phrase
-- Join     Joiner   Phrase
-- Surround Brackets Phrase
-- --
-- Dictated Words  -- ^ the default

data Joiner = Camel | Class | Snake | Dash | File | Squeeze deriving (Show,Eq,Ord,Enum,Typeable)
data Casing = Upper | Lower | Capper deriving (Show,Eq,Ord,Enum,Typeable)
-- data Brackets = Par | Square | Curl | String | Angles deriving (Show,Eq,Ord,Enum,Typeable)

data Brackets = Brackets String String | Bracket Char deriving (Show,Eq,Ord,Typeable)
brackets = 'brackets
 <=> Bracket # "round" & character
 <|> Brackets "(" ")" # "par"
 <|> Brackets "[" "]" # "square"
 <|> Brackets "{" "}" # "curl"
 <|> Brackets "<" ">" # "angles"
 <|> Bracket '"' # "string"
 <|> Bracket '|' # "norm"

character :: Command Char
character = 'character <=> undefined


data Click = Click Times Button deriving (Show,Eq)
click :: Command Click
click = 'click <=>
 Click # optionalEnum times & optionalEnum button & "click"
 -- type inference with the {&} sugar even works for:
 --  Click # optionalEnum enumCommand & optionalEnum enumCommand & "click"
 -- the terminal "click" makes the grammar "non-canonical" i.e.
 --  where product types are merged with <*> (after "lifting" into RHS)
 --  and sum types are merged with <|> (after "tagging" with the constructor)

data Times = Single | Double | Triple deriving (Show,Eq,Enum,Typeable)
times = enumCommand :: Command Times

data Button = LeftButton | MiddleButton | RightButton deriving (Show,Eq,Enum,Typeable)
button :: Command Button
button = qualifiedCommand

newtype Positive = Positive Int deriving (Show,Eq)
positive :: Command Positive
positive = 'positive
 <=> Positive # (asum . fmap int) [1..9]

newtype Dictation = Dictation [String] deriving (Show,Eq)
dictation :: Command Dictation
dictation = specialCommand 'dictation
 (DNSGrammar (DNSProduction (DNSRule name)
                            (DNSNonTerminal (SomeDNSLHS (DNSBuiltinRule DGNDictation)) :| [])) [] [])
 (\context -> Dictation <$> anyBlack `manyUntil` context)
 where
 name = set (dnsMetaInfo.dnsInline) True $ defaultDNSMetaName (unsafeLHSFromName 'dictation)

-- | context-sensitive grammars (like 'dictation') work (?) with 'atom'
data Directions = Directions Dictation Dictation Dictation deriving (Show,Eq)
directions :: Command Directions
directions = 'directions <=> "directions" &> (runPerms $ Directions
 <$> atom ("from" &> dictation)
 <*> atom ("to"   &> dictation)
 <*> atom ("by"   &> dictation))

-- | context-free grammars (like from 'twig' or 'anyWord') can use 'maybeAtomR'
data Directions_ = Directions_ (Maybe Place) (Maybe Place) (Maybe Transport) deriving (Show,Eq)
directions_ :: Command Directions_
directions_ = 'directions_ <=> "directions" &> (runPerms $ Directions_
 <$> maybeAtomR ("from" &> place)
 <*> maybeAtomR ("to"   &> place)
 <*> maybeAtomR ("by"   &> transport))

-- data Directions__ = Directions__ (Maybe Dictation) (Maybe Dictation) (Maybe Dictation) deriving (Show,Eq)
-- directions__ :: Command Directions__
-- directions__ = 'directions__ <=> "directions" &> (runPerms $ Directions__
 -- <$> maybeAtomR ("from" &> dictation)
 -- <*> maybeAtomR ("to"   &> dictation)
 -- <*> maybeAtomR ("by"   &> dictation))


newtype Place = Place String deriving (Show,Eq)
place = 'place <=> Place # vocabulary ["here","there"]

data Transport = Foot | Bike | PublicTransit | Car deriving (Show,Ord,Eq,Enum,Typeable)
transport = enumCommand

exampleDirections = fmap (unwords . words)
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


oneSecond = round (1e6 :: Double) :: Int

attemptAsynchronously action = do
 (timeout oneSecond action) `withAsync` (waitCatch >=> \case
   Left error     -> print error
   Right Nothing  -> putStrLn "..."
   Right (Just _) -> return ()
  )

attempt = attemptAsynchronously

attemptParse command = attempt . handleParse command

-- attemptSerialize (Command _ g _) = attempt $ either print print $ (serializeProduction (renderProduction g))
attemptSerialize command = attempt $ either print T.putStrLn $ serialized command

attemptNameRHS = attempt . print . showLHS . unsafeLHSFromRHS


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

 -- putStrLn ""
 -- attempt . print . renders $ root










 putStrLn ""
 attemptSerialize root -- timed out. fast after Commands.Commands.Sugar, I think. Theory: may be left associated tree is efficient, wall arbitrarily associated free alternatives is obscenely polynomial inefficient. But even for such a small grammar? May be non-left association causes non-termination?
 -- I don't think so: {<|> Repeat     <$> liftCommand positive <*> liftCommand root} still terminates in both the serialization in the parsing. Maybe because all the alternatives (or their children) were not left associated? I don't know
 -- See also: attemptParse (multipleC root) "no 1 replace this and that with that and this"



 putStrLn ""
 attemptParse positive "9"
 attemptParse dictation "this and that"
 attemptParse root "no"
 attemptParse root "no way"
 attemptParse root "replace this and that with that and this"
 attemptParse root "1 1 no"
 attemptParse root "say 638 Pine St., Redwood City 94063"
 attemptParse root "no BAD"     -- prefix succeeds, but the whole should fail

 attemptParse (multipleC root) "no no"
 attemptParse (multipleC root) "no replace this with that"
 attemptParse (multipleC root) "no 1 replace this with that"
 attemptParse (multipleC root) "no 1 replace this and that with that and this" -- timed out. left recursion?

 putStrLn ""
 attemptParse click "single left click"
 attemptParse click "left click"
 attemptParse click "single click"
 attemptParse click "click"

 putStrLn ""
 -- attemptParse phrase



 putStrLn ""
 attemptNameRHS ("from" &> place)
 attemptNameRHS ("to"   &> place)
 attemptNameRHS ("by"   &> transport)



 putStrLn ""
 attemptSerialize directions
 putStrLn ""
 traverse_ (attemptParse directions) exampleDirections
 attemptParse directions "directions from Redwood City to San Francisco by public transit"

 putStrLn ""
 attemptSerialize directions_
 putStrLn ""
 traverse_ (attemptParse directions_) exampleDirections

 -- putStrLn ""
 -- attemptSerialize directions__
 -- putStrLn ""
 -- traverse_ (attemptParse directions__) exampleDirections
 -- attemptParse directions__ "directions to San Francisco by public transit from Redwood City"

 putStrLn ""
 print (getWords . _grammar $ button)

 putStrLn ""
 print $ cycles $
  [ ("non recursive",        "N", [])
  , ("self recursive",       "S", ["S"])
  , ("mutually recursive A", "A", ["B"])
  , ("mutually recursive B", "B", ["A","C"])
  , ("mutually recursive C", "C", ["A","S","N"])
  ]

 putStrLn ""
 print $ SomeDNSLHS (DNSList "n")
 print $ DNSNonTerminal (SomeDNSLHS (DNSList "n"))
 print $ DNSOptional (DNSNonTerminal (SomeDNSLHS (DNSList "n")))

 putStrLn ""
 let name = DNSNonTerminal (SomeDNSLHS (DNSList "A"))
 let rhs = DNSAlternatives $ fromList [ name, DNSSequence $ fromList [DNSTerminal (DNSToken "t"), DNSNonTerminal (SomeDNSLHS (DNSList "B")), DNSOptional (DNSMultiple name)] ]
 print $ transform (\case
   DNSNonTerminal ((== (SomeDNSLHS (DNSList "A"))) -> True) -> DNSNonTerminal (SomeDNSLHS (DNSList "XXX"))
   r -> r)
  rhs


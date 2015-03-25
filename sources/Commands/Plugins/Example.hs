{-# LANGUAGE DeriveDataTypeable, ExtendedDefaultRules, LambdaCase #-}
{-# LANGUAGE NamedFieldPuns, PatternSynonyms, RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TupleSections  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind -fno-warn-orphans -fno-warn-unused-imports -fno-warn-type-defaults #-}
module Commands.Plugins.Example where
import           Commands

import           Control.Applicative             hiding (many, optional)
import           Control.Applicative.Permutation
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Lens                    hiding (( # ), (&))
import           Control.Monad                   (void, (<=<), (>=>))
import           Control.Monad.Catch             (catches)
import           Control.Parallel
import           Data.Bitraversable
import           Data.Either                     (either)
import           Data.Foldable                   (Foldable (..), asum,
                                                  traverse_)
import           Data.List.NonEmpty              (NonEmpty (..), fromList)
import qualified Data.Text.Lazy.IO               as T
import           Data.Typeable
import           Numeric.Natural                 ()
import           Prelude                         hiding (foldr)
import           System.Timeout                  (timeout)
import           Text.PrettyPrint.Leijen.Text    hiding (brackets, empty, int,
                                                  (<$>), (<>))


data Root
 = Repeat Positive Root
 | Edit Action Region
 | Undo
 | ReplaceWith Dictation Dictation
 | Click_ Click
 | Phrase_ Phrase
 -- Roots [Root]
 deriving (Show,Eq)

root = set comExpand 1 $ 'root
 <=> empty
 <|> Repeat      # positive & root
 <|> ReplaceWith # "replace" & dictation & "with" & dictation
 <|> Undo        # "no"         -- order matters..
 <|> Undo        # "no way"     -- .. the superstring "no way" should come before the substring "no" (unlike this example)
 <|> Click_      # click
 <|> Edit        # action    & region
 <|> Phrase_     # phrase
 -- <|> Roots       # (multipleC root)

data Action = Copy | Delete | Next deriving (Show,Eq,Enum,Typeable)
action = enumCommand

data Region = Char | Word | Line deriving (Show,Eq,Enum,Typeable)
region = enumCommand

-- Action = Pick | Go |
-- Region = Selection |
-- Direction = Whole | Backwards | Forwards





data Phrase
 = Verbatim   Dictation
 | Escaped    Keyword Phrase
 | Quoted     Dictation Phrase

 | Pressed [Key] (Maybe Phrase)
 | Spelled [Char] (Maybe Phrase)
 | Letter  Char (Maybe Phrase)
 | Cap     Char (Maybe Phrase)

 | Case     Casing   Phrase
 | Join     Joiner   Phrase
 | Surround Brackets Phrase

 | Dictated Dictation
 deriving (Show,Eq,Ord)

phrase = set comExpand 3 $ 'phrase

 <=> Verbatim # "say" & dictation
 <|> Escaped  # "lit" & keyword & phrase
 <|> Quoted   # "quote" & dictation & "unquote" & phrase

 <|> Pressed  # "press" & many key & optional phrase
 <|> Spelled  # "spell" & many character & optional phrase
 <|> Spelled  #           many character & optional phrase
 <|> Letter   # character & optional phrase
 <|> Cap      # "cap" & character & optional phrase

 <|> Case     # casing   & phrase
 <|> Join     # joiner   & phrase
 <|> Surround # brackets & phrase

 <|> Dictated # dictation

speech = phrase -- TODO

data Joiner = Camel | Class | Snake | Dash | File | Squeeze deriving (Show,Eq,Ord,Enum,Typeable)
joiner = enumCommand

data Casing = Upper | Lower | Capper deriving (Show,Eq,Ord,Enum,Typeable)
casing = enumCommand

-- data Brackets = Par | Square | Curl | String | Angles deriving (Show,Eq,Ord,Enum,Typeable)
data Brackets = Brackets String String | Bracket Char deriving (Show,Eq,Ord,Typeable)
brackets = 'brackets
 <=> bracket          # "round" & character
 <|> Brackets "(" ")" # "par"
 <|> Brackets "[" "]" # "square"
 <|> Brackets "{" "}" # "curl"
 <|> Brackets "<" ">" # "angles"
 <|> bracket '"'      # "string"
 <|> bracket '|'      # "norm"

bracket c = Brackets [c] [c]

character :: Command Char
character = 'character <=> empty

 <|> '`' # "grave"
 <|> '~' # "till"
 <|> '!' # "bang"
 <|> '@' # "axe"
 <|> '#' # "pound"
 <|> '$' # "doll"
 <|> '%' # "purse"
 <|> '^' # "care"
 <|> '&' # "amp"
 <|> '*' # "star"
 <|> '(' # "lore"
 <|> ')' # "roar"
 <|> '-' # "dash"
 <|> '_' # "score"
 <|> '=' # "eek"
 <|> '+' # "plus"
 <|> '[' # "lack"
 <|> '{' # "lace"
 <|> ']' # "rack"
 <|> '}' # "race"
 <|> '\\' # "stroke"
 <|> '|' # "pipe"
 <|> ';' # "sem"
 <|> ':' # "colon"
 <|> '\'' # "tick"
 <|> '"' # "quote"
 <|> ',' # "com"
 <|> '<' # "less"
 <|> '.' # "dot"
 <|> '>' # "great"
 <|> '/' # "slash"
 <|> '?' # "quest"
 <|> ' ' # "ace"
 <|> '\t' # "tab"
 <|> '\n' # "line"

 <|> '0' # "zero"
 <|> '1' # "one"
 <|> '2' # "two"
 <|> '3' # "three"
 <|> '4' # "four"
 <|> '5' # "five"
 <|> '6' # "six"
 <|> '7' # "seven"
 <|> '8' # "eight"
 <|> '9' # "nine"

 <|> 'a' # "ay"
 <|> 'b' # "bee"
 <|> 'c' # "sea"
 <|> 'd' # "dee"
 <|> 'e' # "eek"
 <|> 'f' # "eff"
 <|> 'g' # "gee"
 <|> 'h' # "aych"
 <|> 'i' # "eye"
 <|> 'j' # "jay"
 <|> 'k' # "kay"
 <|> 'l' # "el"
 <|> 'm' # "em"
 <|> 'n' # "en"
 <|> 'o' # "oh"
 <|> 'p' # "pea"
 <|> 'q' # "queue"
 <|> 'r' # "are"
 <|> 's' # "ess"
 <|> 't' # "tea"
 <|> 'u' # "you"
 <|> 'v' # "vee"
 <|> 'w' # "dub"
 <|> 'x' # "ex"
 <|> 'y' # "why"
 <|> 'z' # "zee"

-- | 'Key's and 'Char'acters are "incomparable":
--
-- * many modifiers are keys that aren't characters (e.g. 'CommandKey')
-- * many nonprintable characters are not keys (e.g. @\'\\0\'@)
--
-- so we can't embed the one into the other, but we'll just keep things simple with duplication.
--
type Key = Char -- TODO
key :: Command Key
key = 'key <=> empty

type Keyword = Word -- TODO
keyword :: Command Keyword
keyword = 'keyword <=> empty

type Separator = String -- TODO



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
button = qualifiedCommand

newtype Positive = Positive Int deriving (Show,Eq)
positive = 'positive
 <=> Positive # (asum . fmap int) [1..9]



newtype Dictation = Dictation [String] deriving (Show,Eq,Ord)
dictation = specialCommand 'dictation
 empty
 (DNSProduction info (DNSRule name) (DNSNonTerminal (SomeDNSLHS (DNSBuiltinRule DGNDictation))))
 (\context -> Dictation <$> anyBlack `manyUntil` context)
 where
 name = defaultDNSExpandedName (unsafeLHSFromName 'dictation)
 info = set dnsInline True defaultDNSInfo






-- | context-sensitive grammars (like 'dictation') work (?) with 'atom'
data Directions = Directions Dictation Dictation Dictation deriving (Show,Eq)
directions = 'directions <=> "directions" &> (runPerms $ Directions
 <$> atom ("from" &> dictation)
 <*> atom ("to"   &> dictation)
 <*> atom ("by"   &> dictation))

-- | context-free grammars (like from 'twig' or 'anyWord') can use 'maybeAtomR'
data Directions_ = Directions_ (Maybe Place) (Maybe Place) (Maybe Transport) deriving (Show,Eq)
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



data Even = Even (Maybe Odd)  deriving Show
data Odd  = Odd  (Maybe Even) deriving Show
odd_  = set comExpand 1 $ 'odd_  <=> Odd  # "odd"  & optional even_
even_ = set comExpand 1 $ 'even_ <=> Even # "even" & optional odd_



-- it seems to be synchronous, even with threaded I guess?
attemptAsynchronously :: Int -> IO () -> IO ()
attemptAsynchronously seconds action = do
 (timeout (seconds * round 1e6) action) `withAsync` (waitCatch >=> \case
   Left error     -> print error
   Right Nothing  -> putStrLn "..."
   Right (Just _) -> return ()
  )

attempt = attemptAsynchronously 1

attemptParse command = attempt . handleParse command

attemptSerialize command = attemptAsynchronously 3 $ either print T.putStrLn $ serialized command

attemptNameRHS = attempt . print . showLHS . unsafeLHSFromRHS


main = do

 putStrLn ""
 attemptSerialize root -- timed out. fast after Commands.Commands.Sugar, I think. Theory: may be left associated tree is efficient, wall arbitrarily associated free alternatives is obscenely polynomial inefficient. But even for such a small grammar? May be non-left association causes non-termination?
 -- I don't think so: {<|> Repeat     <$> liftCommand positive <*> liftCommand root} still terminates in both the serialization in the parsing. Maybe because all the alternatives (or their children) were not left associated? I don't know
 -- See also: attemptParse (multipleC root) "no 1 replace this and that with that and this"
 -- print $ stronglyConnComp . fmap dnsAdjacency . getDescendentProductions $ root

 -- putStrLn ""
 -- attemptParse positive "9"
 -- attemptParse dictation "this and that"
 -- attemptParse root "no"
 -- attemptParse root "no way"
 -- attemptParse root "replace this and that with that and this"
 -- attemptParse root "1 1 no"
 -- attemptParse root "say 638 Pine St., Redwood City 94063"
 -- attemptParse root "no BAD"     -- prefix succeeds, but the whole should fail

 -- attemptParse (multipleC root) "no no"
 -- attemptParse (multipleC root) "no replace this with that"
 -- attemptParse (multipleC root) "no 1 replace this with that"
 -- attemptParse (multipleC root) "no 1 replace this and that with that and this" -- timed out. left recursion?

 -- putStrLn ""
 -- attemptParse click "single left click"
 -- attemptParse click "left click"
 -- attemptParse click "single click"
 -- attemptParse click "click"

 putStrLn ""
 -- attemptParse phrase



 -- putStrLn ""
 -- attemptNameRHS ("from" &> place)
 -- attemptNameRHS ("to"   &> place)
 -- attemptNameRHS ("by"   &> transport)



 -- putStrLn ""
 -- attemptSerialize directions
 -- putStrLn ""
 -- traverse_ (attemptParse directions) exampleDirections
 -- attemptParse directions "directions from Redwood City to San Francisco by public transit"

 putStrLn ""
 attemptSerialize directions_
 -- putStrLn ""
 -- traverse_ (attemptParse directions_) exampleDirections

 -- putStrLn ""
 -- print (getWords . _comGrammar $ button)

 -- putStrLn ""
 -- print $ cycles $
 --  [ ("non recursive",        "N", [])
 --  , ("self recursive",       "S", ["S"])
 --  , ("mutually recursive A", "A", ["B"])
 --  , ("mutually recursive B", "B", ["A","C"])
 --  , ("mutually recursive C", "C", ["A","S","N"])
 --  ]

 -- putStrLn ""
 -- print $ SomeDNSLHS (DNSList "n")
 -- print $ DNSNonTerminal (SomeDNSLHS (DNSList "n"))
 -- print $ DNSOptional (DNSNonTerminal (SomeDNSLHS (DNSList "n")))

 -- putStrLn ""
 -- let name = DNSNonTerminal (SomeDNSLHS (DNSList "A"))
 -- let rhs = DNSAlternatives $ fromList [ name, DNSSequence $ fromList [DNSTerminal (DNSToken "t"), DNSNonTerminal (SomeDNSLHS (DNSList "B")), DNSOptional (DNSMultiple name)] ]
 -- print $ transform (\case
 --   DNSNonTerminal ((== (SomeDNSLHS (DNSList "A"))) -> True) -> DNSNonTerminal (SomeDNSLHS (DNSList "XXX"))
 --   r -> r)
 --  rhs

 -- putStrLn ""
 -- attemptParse phrase "parens snake upper some words" -- lol "ens"
 -- attemptParse phrase "par snake upper some words"
 -- attemptParse phrase "say some words"
 -- attemptParse phrase "par quote par snake unquote snake some words"
 -- attemptParse phrase "spell grave zero ay bee sea some words"
 -- attemptParse phrase "spell grave zero ay bee sea"
 -- attemptParse phrase "string some words"

 -- putStrLn ""
 -- attemptParse speech "lore some words roar"

 putStrLn ""
 attemptParse even_ "even odd even"
 attemptSerialize odd_
 attemptSerialize even_

 -- putStrLn ""
 -- traverse_ print $ [odd_ ^. comLHS, even_ ^. comLHS]
 -- putStrLn ""
 -- traverse_ print $ (^.. each.dnsProductionLHS.dnsLHSName.dnsExpandedName) (getDescendentProductions odd_)
 -- putStrLn ""
 -- traverse_ print $ (^.. each.dnsProductionLHS.dnsLHSName.dnsExpandedName) (getDescendentProductions even_)
 putStrLn ""
 traverse_ print $ (^.. each.dnsProductionLHS.dnsLHSName.dnsExpandedName) (getDescendentProductions $ optional even_)
 putStrLn ""
 traverse_ print $ (\(Some command) -> command ^? comGrammar.dnsProductionLHS.dnsLHSName.dnsExpandedName) <$> (reifyCommand $ even_)
 putStrLn ""
 traverse_ print $ (\(Some command) -> command ^? comGrammar.dnsProductionLHS.dnsLHSName.dnsExpandedName) <$> (reifyCommand $ optional even_)
 

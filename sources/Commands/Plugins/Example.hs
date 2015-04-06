{-# LANGUAGE DeriveDataTypeable, ExtendedDefaultRules, LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns, PatternSynonyms, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TupleSections          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind -fno-warn-orphans -fno-warn-unused-imports -fno-warn-type-defaults #-}

module Commands.Plugins.Example where
import           Commands.Backends.OSX           hiding (Command)
import qualified Commands.Backends.OSX           as OSX
import           Commands.Core                   hiding (tab)
import           Commands.Frontends.Dragon13

import           Control.Applicative.Permutation
import           Control.Concurrent.Async
import           Control.Lens                    hiding (from, ( # ), (&))
import           Control.Monad.Catch             (SomeException, catches)
import           Data.Bifunctor                  (second)
import           Data.Bitraversable
import           Data.List.NonEmpty              (NonEmpty (..), fromList)
import qualified Data.Text.Lazy                  as T
import qualified Data.Text.Lazy.IO               as T
import           Data.Typeable
import           Language.Python.Version2.Parser (parseModule)
import           Numeric.Natural                 ()
import           Text.PrettyPrint.Leijen.Text    hiding (brackets, empty, int,
                                                  (<$>), (<>))

import           Control.Applicative             hiding (many, optional)
import           Control.Concurrent
import           Control.Monad                   (replicateM_, void, (<=<),
                                                  (>=>))
import           Control.Parallel
import           Data.Char                       (toUpper)
import           Data.Either                     (either)
import           Data.Foldable                   (Foldable (..), asum,
                                                  traverse_)
import           Data.List                       (intercalate)
import           Data.Monoid                     ((<>))
import           Prelude                         hiding (foldr)
import           System.Timeout                  (timeout)


data Root
 = Repeat Positive Root
 | Edit_ Edit
 | Undo
 | ReplaceWith Phrase Phrase
 | Click_ Click
 | Phrase_ Phrase
 -- Roots [Root]
 deriving (Show,Eq)

root :: Command Root
root = set (comGrammar.gramExpand) 1 $ 'root
 <=> empty
 <|> Repeat      # positive & root -- recursive is okay, only left recursion causes non-termination
 <|> ReplaceWith # "replace" & phrase & "with" & phrase
 <|> Undo        # "no"         -- order matters..
 <|> Undo        # "no way"     -- .. the superstring "no way" should come before the substring "no" (unlike this example)
 <|> Click_      # click
 <|> Edit_       # edit
 <|> Phrase_     # phrase
 -- <|> Roots       # (multipleC root)
 <%> \case

  ReplaceWith this that -> \case
   "emacs" -> do
    press met r
    slot (munge this)
    slot (munge that)
   "intellij" -> do
    press met r
    insert (munge this) >> press [] tab
    slot (munge that)
   _ -> nothing

  Undo -> \case
   _ -> press met z

  Edit_ e -> \case
   "emacs" -> editEmacs e
   _ -> nothing

  Repeat n c -> \case
   _ -> replicateM_ (getPositive n) (root `compiles` c)

  _ -> \case
   _ -> nothing

nothing = return ()
press = sendKeyPress
met = [OSX.Command]
r = RKey
z = ZKey
tab = TabKey
slot s = do
 insert s
 press [] ReturnKey

munge :: Phrase -> String
munge = spaceCase . mungePhrase
-- TODO spaces between wor ds, not between letters and punctuation etc. what controls this?

data Edit = Edit Action Region deriving (Show,Eq,Ord)
edit = 'edit <=> empty
-- TODO defaults <|> Edit # action & region
 <|> Edit undefined # region
 <|> flip Edit undefined # action
 <|> Edit # action & region
-- TODO ensure no alternative is empty
-- <|> (\a -> Edit a undefined) # action

data Action = Copy | Delete | Next deriving (Show,Eq,Ord,Enum,Typeable)
action = enumGrammar

data Region = Char | Word | Line deriving (Show,Eq,Ord,Enum,Typeable)
region = enumGrammar

-- Action = Pick | Go |
-- Region = Selection |
-- Direction = Whole | Backwards | Forwards


editEmacs :: Edit -> Actions ()
editEmacs = undefined



data Phrase
 = Verbatim   Dictation
 | Escaped    Keyword Phrase
 | Quoted     Dictation Phrase

 -- TODO | Pressed [Key] (Maybe Phrase)
 | Spelled [Char] (Maybe Phrase)
 | Letter  Char (Maybe Phrase)
 | Cap     Char (Maybe Phrase)

 | Case     Casing   Phrase
 | Join     Joiner   Phrase
 | Surround Brackets Phrase

 | Dictated Dictation (Maybe Phrase)
 deriving (Show,Eq,Ord)

phrase = set gramExpand 3 $ 'phrase

 <=> Verbatim # "say" & dictation
 <|> Escaped  # "lit" & keyword & phrase
 <|> Quoted   # "quote" & dictation & "unquote" & phrase

 -- <|> Pressed  # "press" & many key & optional phrase
 <|> Spelled  # "spell" & many character & optional phrase
 <|> Spelled  #           many character & optional phrase
 <|> Letter   # character & optional phrase
 <|> Cap      # "cap" & character & optional phrase

 <|> Case     # casing   & phrase
 <|> Join     # joiner   & phrase
 <|> Surround # brackets & phrase

 <|> Dictated # dictation & optional phrase

speech = phrase -- TODO

data Casing = Upper | Lower | Capper deriving (Show,Eq,Ord,Enum,Typeable)
casing = enumGrammar

data Joiner = Joiner String | CamelJoiner | ClassJoiner deriving (Show,Eq,Ord)
-- not {JoinerFunction ([String] -> String)} to keep the {Eq} instance for caching
-- fake equality? {JoinerFunction Name ([String] -> String)} so {JoinerFunction 'camelCase camelCase}
-- maybe some type from data.split package, that both supports decidable equality and that can build functions
joiner = 'joiner
 <=> (\c -> Joiner [c]) # "join" & character
 <|> Joiner "_" # "snake"
 <|> Joiner "-" # "dash"
 <|> Joiner "/" # "file"
 <|> Joiner ""  # "squeeze"
 <|> CamelJoiner # "camel"
 <|> ClassJoiner # "class"

data Brackets = Brackets String String deriving (Show,Eq,Ord,Typeable)
brackets = 'brackets
 <=> bracket          # "round" & character
 <|> Brackets "(" ")" # "par"
 <|> Brackets "[" "]" # "square"
 <|> Brackets "{" "}" # "curl"
 <|> Brackets "<" ">" # "angle"
 <|> bracket '"'      # "string"
 <|> bracket '\''     # "ticked"
 <|> bracket '|'      # "norm"
 -- <|> Brackets "**" "**" # "bold"

bracket c = Brackets [c] [c]



mungePhrase :: Phrase -> [String]
mungePhrase = \case
 Verbatim   (Dictation ws) -> ws
 Escaped    kw p -> kw : mungePhrase p  -- TODO can a keyword be multiple words?
 Quoted     (Dictation ws) p -> ws <> mungePhrase p

 Spelled cs mp -> (:[]) `fmap` cs <> maybe [] mungePhrase mp  -- TODO singleton or grouped?
 Letter  c  mp -> [c]              : maybe [] mungePhrase mp
 Cap     c  mp -> [toUpper c]      : maybe [] mungePhrase mp

 -- TODO inside-out or outside-in? I.e. forwards or backwards?
 -- outside-in is easier to implement, inside-out is perhaps more intuitive to speak.
 Case     casing   p -> caseWith casing <$> mungePhrase p
 Join     joiner   p -> [joinWith joiner $ mungePhrase p]
 Surround brackets p -> surroundWith brackets $ mungePhrase p

 Dictated (Dictation ws) mp -> ws <> maybe [] mungePhrase mp

caseWith :: Casing -> (String -> String)
caseWith = \case
 Upper  -> upper
 Lower  -> lower
 Capper -> capitalize

joinWith :: Joiner -> ([String] -> String)
joinWith = \case
 Joiner s -> intercalate s
 CamelJoiner -> camelCase
 ClassJoiner -> classCase

surroundWith :: Brackets -> ([String] -> [String])
surroundWith (Brackets l r) ss = [l] <> ss <> [r]


character :: Grammar Char
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

 <|> 'a' # "A"  -- TODO What can we get back from Dragon anyway?
 <|> 'b' # "B"
 <|> 'c' # "C"

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
-- * many modifiers are keys that aren't characters (e.g. 'GrammarKey')
-- * many nonprintable characters are not keys (e.g. @\'\\0\'@)
--
-- so we can't embed the one into the other, but we'll just keep things simple with duplication.
--
key :: Grammar Key
key = 'key <=> empty

type Keyword = String -- TODO
keyword :: Grammar Keyword
keyword = 'keyword <=> empty

type Separator = String -- TODO



data Click = Click Times Button deriving (Show,Eq)
click :: Grammar Click
click = 'click <=>
 Click # optionalEnum times & optionalEnum button & "click"
 -- type inference with the {&} sugar even works for:
 --  Click # optionalEnum enumGrammar & optionalEnum enumGrammar & "click"
 -- the terminal "click" makes the grammar "non-canonical" i.e.
 --  where product types are merged with <*> (after "lifting" into RHS)
 --  and sum types are merged with <|> (after "tagging" with the constructor)

data Times = Single | Double | Triple deriving (Show,Eq,Enum,Typeable)
times = enumGrammar :: Grammar Times

data Button = LeftButton | MiddleButton | RightButton deriving (Show,Eq,Enum,Typeable)
button = qualifiedGrammar

positive = 'positive
 <=> Positive # (asum . fmap int) [1..9]



newtype Dictation = Dictation [String] deriving (Show,Eq,Ord)
dictation = dragonGrammar 'dictation
 (DNSNonTerminal (SomeDNSLHS (DNSBuiltinRule DGNDictation)))
 (\context -> Dictation <$> anyBlack `manyUntil` context)

newtype Letters = Letters [Char] deriving (Show,Eq,Ord)
letters = dragonGrammar 'letters
 (DNSNonTerminal (SomeDNSLHS (DNSBuiltinRule DGNLetters)))
 (\context -> Letters <$> anyLetter `manyUntil` context) -- TODO greedy (many) versus non-greedy (manyUntil)

-- |
-- TODO spacing, casing, punctuation; are all weird when letters are recognized by Dragon NaturallySpeaking.
anyLetter = anyChar




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
-- directions__ :: Grammar Directions__
-- directions__ = 'directions__ <=> "directions" &> (runPerms $ Directions__
 -- <$> maybeAtomR ("from" &> dictation)
 -- <*> maybeAtomR ("to"   &> dictation)
 -- <*> maybeAtomR ("by"   &> dictation))


newtype Place = Place String deriving (Show,Eq)
place = 'place <=> Place # vocabulary ["here","there"]

data Transport = Foot | Bike | PublicTransit | Car deriving (Show,Ord,Eq,Enum,Typeable)
transport = enumGrammar

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
odd_  = set gramExpand 1 $ 'odd_  <=> Odd  # "odd"  & optional even_
even_ = set gramExpand 1 $ 'even_ <=> Even # "even" & optional odd_


data Even2 = Even2 [Odd2]  deriving Show
data Odd2  = Odd2  [Even2] deriving Show
odd2  = set gramExpand 1 $ 'odd2  <=> Odd2  # "odd"  & multiple even2
even2 = set gramExpand 1 $ 'even2 <=> Even2 # "even" & multiple odd2

-- data Test = Test deriving (Show,Eq,Enum,Typeable)
-- test = enumGrammar
test = 'test <=> id # multiple transport



-- it seems to be synchronous, even with threaded I guess?
attemptAsynchronously :: Int -> IO () -> IO ()
attemptAsynchronously seconds action = do
 (timeout (seconds * round 1e6) action) `withAsync` (waitCatch >=> \case
   Left error     -> print error
   Right Nothing  -> putStrLn "..."
   Right (Just _) -> return ()
  )

attempt = attemptAsynchronously 1

-- attemptCompile command text = do
--  a <- attemptParse (view comGrammar root) text
--  return $ (view comCompiler root) a

attemptMunge' text = do
 let a = phrase `parses` text :: Either SomeException Phrase
 print $ second munge a

attemptMunge text = do
 case phrase `parses` text :: Either SomeException Phrase of
  Left e  -> print e
  Right p -> do
   putStrLn ""
   print p
   print $ munge p

attemptParse grammar = attempt . handleParse grammar

attemptSerialize grammar = attemptAsynchronously 3 $ either print printSerializedGrammar $ serialized grammar

attemptNameRHS = attempt . print . showLHS . unsafeLHSFromRHS

printSerializedGrammar SerializedGrammar{..} = do
 replicateM_ 3 $ putStrLn ""
 T.putStrLn $ display serializedRules
 putStrLn ""
 T.putStrLn $ display serializedLists



main = do

 -- putStrLn ""
 -- attemptParse positive "9"
 -- attemptParse dictation "this and that"
 -- attemptParse root "no"
 -- attemptParse root "no way"
 -- attemptParse root "replace this and that with that and this"
 -- attemptParse root "1 1 no"
 -- attemptParse root "say 638 Pine St., Redwood City 94063"
 -- attemptParse root "no BAD"     -- prefix succeeds, but the whole should fail

 -- attemptParse (multipleC root) "no no 1 replace this and that with that and this"

 -- putStrLn ""
 -- attemptParse click "single left click"
 -- attemptParse click "left click"
 -- attemptParse click "single click"
 -- attemptParse click "click"

 -- putStrLn ""
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

 -- putStrLn ""
 -- attemptSerialize directions_
 -- putStrLn ""
 -- traverse_ (attemptParse directions_) exampleDirections

 -- putStrLn ""
 -- print (getWords . _gramGrammar $ button)

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

 -- putStrLn ""
 -- attemptParse even_ "even odd even"
 -- attemptSerialize even_

 -- putStrLn ""
 -- attemptParse even2 "even odd even"
 -- attemptSerialize even2

 -- attemptSerialize root

 -- putStrLn ""
 -- let theShim = ShimR "'''_'''" "{'_':'_'}" "'_'" "'_'"
 -- putStrLn $ getShim theShim
 -- writeShim "shim.py" theShim
 -- print $ take 30 $ show $ parseModule (getShim theShim) ""


 -- PythonFile pf <- shimmySerialization (T.pack "'http://192.168.56.1:8080'") sg
 -- T.putStrLn $ pf
 -- -- writeFile "_shim.py" (T.unpack pf)


 -- putStrLn ""
 -- -- attemptSerialize test
 -- attemptSerialize (view comGrammar root)
 -- let Right sg = serialized test  -- TODO why does the unary Test fail? Optimization?
 -- let addresses = (Address ("'192.168.56.1'") ("8080"), Address ("'192.168.56.101'") ("8080"))
 -- PythonFile pf <- shimmySerialization addresses sg
 -- runActions $ setClipboard (T.unpack pf)
 -- T.putStrLn $ pf


 putStrLn ""
 -- attemptParse (view comGrammar root) "replace this and that with that and this"
 -- TODO verify phrase/munge with more tests
 attemptMunge "par round grave camel with async action spell A B C"  -- (`withAsyncActionABC`)

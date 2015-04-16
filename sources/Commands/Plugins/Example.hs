{-# LANGUAGE DeriveDataTypeable, ExtendedDefaultRules, ImplicitParams      #-}
{-# LANGUAGE LambdaCase, NamedFieldPuns, PatternSynonyms, PostfixOperators #-}
{-# LANGUAGE RankNTypes, RecordWildCards, ScopedTypeVariables              #-}
{-# LANGUAGE TemplateHaskell, TupleSections                                #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind -fno-warn-orphans -fno-warn-unused-imports -fno-warn-type-defaults #-}

module Commands.Plugins.Example where
import           Commands.Backends.OSX           hiding (Command)
import qualified Commands.Backends.OSX           as OSX
import           Commands.Core                   hiding (tab)
import           Commands.Frontends.Dragon13
import           Commands.Servers.Servant

import           Control.Applicative.Permutation
import           Control.Concurrent.Async
import           Control.Lens                    hiding (from, ( # ), (&))
import           Control.Monad.Catch             (SomeException, catches)
import qualified Data.Aeson                      as J
import           Data.Bifunctor                  (second)
import           Data.Bitraversable
import qualified Data.ByteString.Lazy.Char8      as B
import           Data.List.NonEmpty              (NonEmpty (..), fromList)
import qualified Data.List.NonEmpty              as NonEmpty
import qualified Data.Text.Lazy                  as T
import qualified Data.Text.Lazy.IO               as T
import           Data.Typeable
import           Language.Python.Version2.Parser (parseModule)
import           Numeric.Natural                 ()
import qualified Text.Parsec                     as Parsec
import           Text.PrettyPrint.Leijen.Text    hiding (brackets, empty, int,
                                                  (<$>), (<>))

import           Control.Applicative             hiding (many, optional)
import           Control.Concurrent
import           Control.Monad                   (replicateM_, void, (<=<),
                                                  (>=>))
import           Control.Monad.Reader            (asks)
import           Control.Parallel
import           Data.Char                       (toUpper)
import           Data.Either                     (either)
import           Data.Foldable                   (Foldable (..), asum,
                                                  traverse_)
import qualified Data.List                       as List
import qualified Data.Map                        as Map
import           Data.Monoid                     ((<>))
import           Prelude                         hiding (foldl, foldr1)
import           System.Timeout                  (timeout)
-- import qualified Data.Map as Map (Map)


data Root
 = Repeat Positive Root
 | Edit_ Edit
 | Undo
 | ReplaceWith Phrase Phrase
 | Click_ Click
 | Phrase_ Phrase
 -- Roots [Root]
-- TODO | Frozen freeze
 deriving (Show,Eq)

-- TODO currently throws "grammar too complex" :-(
root :: Command Root
root = set (comGrammar.gramExpand) 1 $ 'root
 <=> empty
 <|> Repeat      # positive & root -- recursive is okay, only left recursion causes non-termination
 <|> ReplaceWith # "replace" & phrase & "with" & phrase-- TODO
 -- TODO <|> ReplaceWith # "replace" & phrase & "with" & (phrase <|>? "blank")
 <|> Undo        # "no"         -- order matters..
 <|> Undo        # "no way"     -- .. the superstring "no way" should come before the substring "no" (unlike this example)
 <|> Click_      # click
 <|> Edit_       # edit
 <|> Phrase_     # "say" & phrase -- TODO remove prefix
 -- <|> Roots       # (multipleC root)
-- TODO <|> Frozen # "freeze" & root
 <%> \case

-- TODO Frozen r -> \case
--    _ -> pretty print the tree of commands, both as a tree and as the flat recognition,
--  (inverse of parsing), rather than executing. For debugging/practicing, and maybe for batching.

  ReplaceWith this that -> \case
   "emacs" -> do
    press met r
    slot =<< munge this
    slot =<< munge that
   "intellij" -> do
    press met r
    (insert =<< munge this) >> press [] tab
    slot =<< munge that
   _ -> nothing


  Undo -> always $ press met z

  Edit_ e -> onlyWhen "emacs" $ editEmacs e

  Repeat n c ->
   \x -> replicateM_ (getPositive n) $ (root `compiles` c) x

  Phrase_ p -> always $ do
   insert =<< munge p

  _ -> always nothing


nothing = return ()
press = sendKeyPress
met = [OSX.Command]
r = RKey
z = ZKey
tab = TabKey
slot s = do
 insert s
 press [] ReturnKey
always = const
when :: [CompilerContext] -> Actions () -> (CompilerContext -> Actions ())
when theseContexts thisAction = \theContext -> do
 if theContext `List.elem` theseContexts
 then thisAction
 else nothing
onlyWhen = when . (:[])
whenEmacs = onlyWhen "emacs"







-- | like the tokens of an s-expression.
data Phrase_
 = Escaped_  Keyword -- ^ atom-like.
 | Quoted_   Dictation -- ^ list-like.
 | Pasted_ -- ^ atom-like.
 | Blank_ -- ^ atom-like.
 | Separated_ Separator -- ^ like a close paren.
 | Cased_      Casing -- ^ function-like (and an "open paren").
 | Joined_     Joiner -- ^ function-like (and an "open paren").
 | Surrounded_ Brackets -- ^ function-like (and an "open paren").
 | Capped_   [Char] -- ^ atom-like.
 | Spelled_  [Char] -- ^ list-like.
 | Dictated_ Dictation -- ^ list-like.
 deriving (Show,Eq,Ord)

-- | its custom parser and grammar are implemented differently, but should behave consistently.
--
-- (its special parser threads the context differently than the generic parser would.)
--
-- TODO erase this horror from time
--
-- transforms "token"s from 'phrase_' into an "s-expression" with 'pPhrase'.
phrase = pPhrase <$> phrase_
phrase_ = Grammar
 (Rule l dependencies)
 (defaultDNSCommandProduction l gP)
 (\context -> pP context)

 where
 Just l = lhsFromName 'phrase
 dependencies = [] <$ liftGrammar (phraseA `eitherG` phraseB `eitherG` phraseC `eitherG` dictation)
 -- TODO the RHS of special grammars are ignored (so the eithers don't matter), except for extracting dependencies for serialization

 pP context                     -- merges the context-free Parsec.many the context-sensitive manyUntil
    = ([]    <$  (case context of Some q -> (try . lookAhead) q *> pure undefined)) -- terminate.
  <|> ((:)   <$> pAB             <*> pP context)  -- continue. e.g. can escape "say" with "lit"
  <|> ((:[]) <$> pC context) -- terminate.
  <|> ((:)   <$> (pDxAB context) <*> pP context)  -- continue
 pAB = pA <|> pB
 pDxAB context = Dictated_ <$> pD (case context of Some q -> Some (pAB <|> (q *> pure undefined)))
 -- pAB context = (pA context <||> pB context)
 -- pD'AB context = ((Right . Dictated) <$> pD) `manyUntil` (pAB <|> context)
 pA         = try $ phraseA   ^. gramParser $ undefined -- context free
 pB         = try $ phraseB   ^. gramParser $ undefined -- context free
 pC context = try $ phraseC   ^. gramParser $ context             -- context-sensitive
 pD context = try $ dictation ^. gramParser $ context            -- context-sensitive
 -- pB' = (Dictated . Dictation) <$> anyWord `manyUntil` pB

 gP = (DNSSequence $ fromList
  [ (DNSOptional . DNSMultiple) (DNSAlternatives $ fromList [gA, gB, gD])
  ,                              DNSAlternatives $ fromList [gC, gB, gD]
  ])
 gA = (DNSNonTerminal . SomeDNSLHS) $ phraseA   ^. gramGrammar.dnsProductionLHS
 gB = (DNSNonTerminal . SomeDNSLHS) $ phraseB   ^. gramGrammar.dnsProductionLHS
 gC = (DNSNonTerminal . SomeDNSLHS) $ phraseC   ^. gramGrammar.dnsProductionLHS
 gD = (DNSNonTerminal . SomeDNSLHS) $ dictation ^. gramGrammar.dnsProductionLHS

-- | a sub-phrase where a phrase to the right is certain.
phraseA = 'phraseA <=> empty
 <|> (Spelled_ . (:[])) # letter_
 <|> (Spelled_ . (:[])) # character
 <|> Escaped_    # "lit" & keyword
 <|> Quoted_     # "quote" & dictation & "unquote"
 <|> Pasted_     # "paste"
 <|> Blank_      # "blank"
 <|> Separated_  # separator
 <|> Cased_      # casing
 <|> Joined_     # joiner
 <|> Surrounded_ # brackets
-- | a sub-phrase where a phrase to the right is possible.
phraseB = 'phraseB <=> empty
 -- TODO letters grammar that consumes tokens with multiple capital letters, as well as tokens with single aliases
 <|> Spelled_  # "spell" & (character-+)
 <|> Capped_   # "caps" & (character-+)
 -- <$> alphabetRHS
-- | a sub-phrase where a phrase to the right is impossible.
phraseC = 'phraseC <=> Dictated_ # "say" & dictation
-- TODO maybe consolidate phrases ABC into a phrase parser, with the same grammar, but which injects different constructors i.e. different views into the same type

newtype Separator = Separator String  deriving (Show,Eq,Ord)
separator = 'separator <=> empty
 <|> Separator ""  # "break"
 <|> Separator " " # "space"
 <|> Separator "," # "comma"

-- | atom-like phrases.
data PAtom
 = Pasted
 | PWord String
 | Acronym [Char]
 | PSep String
 deriving (Show,Eq,Ord)

-- | function-like phrases.
data PFunc
 = PList
 | Cased      Casing
 | Joined     Joiner
 | Surrounded Brackets
 deriving (Show,Eq,Ord)

-- | s-expression-like data.
--
-- 'Phrase_' is like the concrete syntax (tokens, parentheses), while 'Phrase' is like the abstract syntax (s-expressions).
data Phrase
 = PAtom PAtom
 | PSexp PFunc [Phrase]
 -- TODO | PList [Phrase]
 deriving (Show,Eq,Ord)

emptyPhrase :: Phrase
emptyPhrase = PAtom (PWord "")
-- TODO pattern EmptyPhrase, to preserve laws in appendPhrase by pattern matching, under Equality not just Observationally

appendPhrase :: Phrase -> Phrase -> Phrase
appendPhrase x y = PSexp PList [x, y]

-- | a stack over an inlined 'PSexp'. used by 'pPhrase'.
type PStack = NonEmpty (PFunc, [Phrase])

joinSpelled :: [Phrase_] -> [Phrase_]
joinSpelled = foldr' go []
 where
 go :: Phrase_ -> [Phrase_] -> [Phrase_]
 go (Spelled_ xs) (Spelled_ ys : ps) = (Spelled_ $ xs <> ys) : ps
 go p ps = p:ps


pPhrase :: [Phrase_] -> Phrase
pPhrase = fromStack . foldl' go ((PList, []) :| []) . joinSpelled
 -- (PSexp (PList [PAtom (PWord "")]))
 where
 go :: PStack -> Phrase_ -> PStack
 go ps = \case
  (Escaped_  (x)) -> update ps $ PAtom (PWord x)
  (Quoted_   (Dictation xs)) -> update ps $ PSexp PList ((PAtom . PWord) <$> xs)
  (Dictated_ (Dictation xs)) -> update ps $ PSexp PList ((PAtom . PWord) <$> xs)
  (Capped_   cs) -> update ps $ PAtom (Acronym cs)
  (Spelled_  cs) -> update ps $ PAtom (Acronym cs)
  Pasted_ -> update ps $ PAtom Pasted
  Blank_  -> update ps $ PAtom (PWord "")
  Separated_ (Separator x) -> update (pop ps) $ PAtom (PSep x)
  -- Separated_ Broken -> update (pop ps)
  (Cased_     f)  -> push ps (Cased f)
  (Joined_    f)  -> push ps (Joined f)
  (Surrounded_ f) -> push ps (Surrounded f)
 pop :: PStack -> PStack
 pop ((f,ps):|(q:qs)) = update (q:|qs) (PSexp f ps) -- break from the innermost PFunc, it becomes an argument to the outer PFunc
 pop stack            = stack  -- if too many breaks, just ignore
 push :: PStack -> PFunc -> PStack
 push (p:|ps) f = (f, []) :| (p:ps)
 update :: PStack -> Phrase -> PStack
 update ((f,ps):|qs) p = (f, ps <> [p]) :| qs
 fromStack :: PStack -> Phrase
 fromStack = uncurry PSexp . foldr1 (\(f,ps) (g,qs) -> (f, ps <> [PSexp g qs])) . NonEmpty.reverse
 -- conceptually, right-associate the PFunc's.

-- | "E" for environment, that 'munge' needs.
data MungeE = MungeE { clipboard :: String }
-- data MungeE = MungeE { clipboard :: String, spacing :: Map String String }
type Munged a = MungeE -> a

munge :: Phrase -> Actions String
munge p = (mungePhrase p . MungeE) <$> getClipboard

mungePhrase :: Phrase -> Munged String
mungePhrase p = do
 as <- atomizePhrase p
 x <- traverse mungeAtom as
 return $ spaceCase x

-- TODO spaces between wor ds, not between letters and punctuation etc. what controls this?
-- mungePhrase :: Phrase -> String -> [String]
-- mungePhrase (PAtom a) clipboard = case a of

mungeAtom :: PAtom -> Munged String
mungeAtom = \case
 Pasted -> asks clipboard
 PWord w -> pure w
 Acronym cs -> pure cs
 PSep s -> pure s -- TODO remove

-- |  splats the 'PList' into the @['PAtom']@
atomizePhrase :: Phrase -> Munged [PAtom]
atomizePhrase (PAtom a)   _ = [a]
atomizePhrase (PSexp PList ps) e = concatMap (\p -> atomizePhrase p e) ps
-- atomizePhrase (PSexp PList ps) = do? concatMap (\p -> atomizePhrase p e) ps
atomizePhrase (PSexp f ps) e = [PWord $ applyPhrase ps f e]

-- TODO PSexp (Joined CamelJoiner) [PSexp PList [PAtom (PWord "some"),PAtom (PWord "words")]]]
-- don't collapse PList
applyPhrase :: [Phrase] -> PFunc -> Munged String
applyPhrase ps _f = do
 as <- concat <$> traverse atomizePhrase ps
 case _f of
  PList        -> spaceCase <$> traverse mungeAtom as
  Cased f      -> spaceCase <$> traverse (caseWith f) as
  Joined f     ->                     joinWith f as
  Surrounded f -> squeezeCase <$> surroundWith f as

 -- :: (a -> f b) -> f (t a) -> f (t b)

caseWith :: Casing -> (PAtom -> Munged String)
caseWith = \case
 Upper  -> (pure.upper) <=< mungeAtom
 Lower  -> (pure.lower) <=< mungeAtom
 Capper -> (pure.capitalize) <=< mungeAtom

joinWith :: Joiner -> ([PAtom] -> Munged String)
joinWith = \case
 Joiner s    -> (\as e -> List.intercalate s $ fmap (\a -> mungeAtom a e) as)
 CamelJoiner -> camelAtoms
 ClassJoiner -> classAtoms

camelAtoms :: [PAtom] -> Munged String
camelAtoms [] = pure []
camelAtoms (x:xs) = do
 y <- mungeAtom x
 ys <- classAtoms xs
 return $ lower y <> ys
 -- (<>) <$> lower y <*> ys

classAtoms :: [PAtom] -> Munged String
classAtoms = (pure.squeezeCase) <=< (traverse $ \case
 Pasted     -> capitalize <$> asks clipboard
 PWord w    -> pure $ capitalize w
 Acronym cs -> pure $ upper cs
 PSep s -> pure $ s)
-- TODO distinguish Capped from Acronym to preserve capitalization?

surroundWith :: Brackets -> ([PAtom] -> Munged [String])
surroundWith (Brackets l r) as = do
 xs <- traverse mungeAtom as
 return $ [l] <> xs <> [r]
-- TODO generalize by renaming surround to transform: it shares the type with Interleave
-- e.g. "par thread comma 123" -> (1,2,3)

-- TODO  function that acts on 'PAtom's, lifted to act on Phrases






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
 <|> ':' # "coal"
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
 <|> alphabetRHS


{- | equivalent to:

@
 <|> 'a' # "A"
 <|> 'b' # "B"
 <|> 'c' # "C"
 <|> ...
 <|> 'z' # "Z"
@

-}
alphabetRHS = (asum . List.map (\c -> c <$ liftString [toUpper c]) $ ['a'..'z'])
-- TODO What will we get back from Dragon anyway?

-- | 'Key's and 'Char'acters are "incomparable":
--
-- * many modifiers are keys that aren't characters (e.g. 'CommandKey')
-- * many nonprintable characters are not keys (e.g. @\'\\0\'@)
--
-- so we can't embed the one into the other, but we'll just keep things simple with duplication.
--
key :: Grammar Key -- TODO
key = 'key <=> empty

type Keyword = String -- TODO
keyword :: Grammar Keyword
keyword = 'keyword <=>id#word_






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

word_ = dragonGrammar 'word_
 (DNSNonTerminal (SomeDNSLHS (DNSBuiltinRule DGNWords)))
 (\_ -> anyWord)

letter_ :: Grammar Char
letter_ = dragonGrammar 'letter_
 (DNSNonTerminal (SomeDNSLHS (DNSBuiltinRule DGNLetters)))
 (\_ -> spaced anyLetter)

-- newtype Letters = Letters [Char] deriving (Show,Eq,Ord)
-- letters = (set dnsInline True defaultDNSInfo) $ 'letters <=>
--  Letters # (letter-+)
 -- TODO greedy (many) versus non-greedy (manyUntil)

-- |
-- TODO spacing, casing, punctuation; are all weird when letters are recognized by Dragon NaturallySpeaking.
anyLetter = oneOf ['A'..'Z']
-- anyLetter = anyChar
-- anyLetter = (\c -> c <$ [toUpper c]) <$> ['a'..'z'])









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

attemptMunge_ s = do
 putStrLn ""
 print s
 case phrase_ `parses` s of
  Left e  -> print e
  Right p_ -> do
   let p = pPhrase p_
   print $ p_
   print $ p
   print $ mungePhrase p (MungeE "clipboard contents")

attemptParse grammar s = do
 putStrLn ""
 attempt $ handleParse grammar s

attemptSerialize grammar = attemptAsynchronously 3 $ either print printSerializedGrammar $ serialized grammar

attemptNameRHS = attempt . print . showLHS . unsafeLHSFromRHS

printSerializedGrammar SerializedGrammar{..} = do
 replicateM_ 3 $ putStrLn ""
 T.putStrLn $ display serializedRules
 putStrLn ""
 T.putStrLn $ display serializedLists

attemptCompile c x s = case r `parses` s of
  Left  e -> print e
  Right a -> do
   putStrLn ""
   print a
   putStrLn $ showActions $ (c `compiles` a) x
 where r = c ^. comGrammar

attemptPython g = do
 let Right sg = serialized g
 let addresses = (Address ("'192.168.56.1'") ("8080"), Address ("'192.168.56.101'") ("8080"))
 PythonFile pf <- shimmySerialization addresses sg
 runActions $ setClipboard (T.unpack pf)
 T.putStrLn $ pf
 -- TODO why does the unary Test fail? Optimization?


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


 -- attemptParse even2 "even odd even"
 -- attemptSerialize even2

 -- putStrLn ""
 -- let theShim = ShimR "'''_'''" "{'_':'_'}" "'_'" "'_'"
 -- putStrLn $ getShim theShim
 -- writeShim "shim.py" theShim
 -- print $ take 30 $ show $ parseModule (getShim theShim) ""

 -- PythonFile pf <- shimmySerialization (T.pack "'http://192.168.56.1:8080'") sg
 -- T.putStrLn $ pf
 -- -- writeFile "_shim.py" (T.unpack pf)


 putStrLn ""
 -- attemptSerialize (view comGrammar root)
 attemptSerialize phrase

 putStrLn ""
 -- attemptPython phrase

 -- putStrLn ""
 -- -- attemptParse (view comGrammar root) "replace this and that with that and this"
 -- -- TODO verify phrase/munge with more tests
 -- attemptMunge "par round grave camel with async action spell A B C"  -- (`withAsyncActionABC`)

 -- putStrLn ""
 -- attemptCompile root "emacs" "3 no"
 -- attemptCompile root "emacs" "replace this and that with that and this"
 -- attemptCompile root "emacs" "say par round grave camel with async action spell A B C"

 putStrLn ""
 -- attemptParse root "3 no"
 -- attemptParse root "replace this and that with that and this"
 -- attemptParse phrase "par round grave camel with async action spell A B C"

--  , ""
 putStrLn ""
 traverse_ attemptMunge_
  [ "par round grave camel with async break break action"  -- (`withAsync` action) -- "(`withAsync`action)"
  , "lore grave camel with async grave space action roar"  -- (`withAsync` action) -- "lore grave withAsyncGraveSpaceActionRoar"
  , "coal server space tick local"  -- :server 'local --
  , "camel quote double greater equal unquote spaced equals par double greater equal"  -- doubleGreaterEquals = (>>=) -- "doubleGreaterSpacedEqualEquals(doublegreaterequal)" -- "\"Double>ErEqualUnquote   d equals (double>erequal)"
  , "class unit test spell M T A"  -- UnitTestMTA
  , "class spell M T A bid optimization"  -- MTABidOptimization
  , "spell M T A class bid optimization"  -- MTABidOptimization -- "mta BidOptimization"
  , "class M T A bid optimization"  -- MTABidOptimization
  , "camel M T A bid optimization"  -- mtaBidOptimization -- "mTABidOptimization"
  , "lit say camel say some words"  -- say someWords
  , "upper paste"
  , "camel paste" -- "clipboard contents"
  ]  -- TODO "spaced" only modifies the one token to the right, unlike the other joiners which modify all tokens to the right
 putStrLn ""
 traverse_ (attemptParse $ root^.comGrammar)
  [
  -- , "replace this and that with that and this"  -- "this and that" -> "that and this"
  -- , "replace par round grave camel lit with async break break action with blank"  -- "(`withAsync` action)" -> ""
  ]


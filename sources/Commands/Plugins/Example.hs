{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, ExtendedDefaultRules     #-}
{-# LANGUAGE ImplicitParams, LambdaCase, NamedFieldPuns, PatternSynonyms #-}
{-# LANGUAGE PostfixOperators, RankNTypes, RecordWildCards               #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TupleSections         #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind -fno-warn-orphans -fno-warn-unused-imports -fno-warn-type-defaults #-}

module Commands.Plugins.Example where
import           Commands.Backends.OSX           hiding (Command)
import qualified Commands.Backends.OSX           as OSX
import           Commands.Core
import           Commands.Frontends.Dragon13
import           Commands.Plugins.Example.Phrase
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
 | Move_ Move
 | Phrase_ Phrase
 -- Roots [Root]
-- TODO | Frozen freeze
 deriving (Show,Eq)

-- TODO currently throws "grammar too complex" :-(
root :: Command Root
root = set (comGrammar.gramExpand) 1 $ 'root <=> empty
 <|> Repeat      # positive & root -- recursive is okay for parsec, only left recursion causes non-termination
 <|> ReplaceWith # "replace" & phrase & "with" & phrase-- TODO
 -- TODO <|> ReplaceWith # "replace" & phrase & "with" & (phrase <|>? "blank")
 <|> Undo        # "no"         -- order matters..
 <|> Undo        # "no way"     -- .. the superstring "no way" should come before the substring "no" (unlike this example)
 <|> Click_      # click
 <|> Edit_       # edit
 <|> Move_       # move
 <|> (Phrase_ . pPhrase . (:[])) # phraseC -- has prefix
 <|> Phrase_     # phrase  -- must be last, phrase falls back to wildcard.

 -- <|> Roots       # (multipleC root)
-- TODO <|> Frozen # "freeze" & root

 <%> \case

-- TODO Frozen r -> \case
--    _ -> pretty print the tree of commands, both as a tree and as the flat recognition,
--  (inverse of parsing), rather than executing. For debugging/practicing, and maybe for batching.

  ReplaceWith this that -> \case
   "emacs" -> do
    press M r
    slot =<< munge this
    slot =<< munge that
   "intellij" -> do
    press M r
    (insert =<< munge this) >> press tab
    slot =<< munge that
   _ -> nothing

  Undo -> always $ press met z

  Edit_ a -> onlyWhen "emacs" $ editEmacs a
  Move_ a -> onlyWhen "emacs" $ moveEmacs a

  Repeat n c ->
   \x -> replicateM_ (getPositive n) $ (root `compiles` c) x

  Phrase_ p -> always $ do
   insert =<< munge p

  _ -> always nothing

{-

 Up_
 Down_
 Left_
 Right_
 In_
 Out_

 Select
 Beginning
 End
 Copy
 Cut
 Delete
 Transpose
 Google

 That
 Character
 Word_
 Token
 Group
 Line
 Rectangle
 Block
 Page
 Screen
 Everything
 Definition
 Function_
 Reference
 Structure

-}

runEmacs :: String -> Actions ()
runEmacs interactiveCommand = do
 press C w -- non-standard: make this configurable?
 delay 30
 insert interactiveCommand
-- configurable by actions being a ReaderT? The solved another problem, delays or something. code just let the user define it, after copying and pasting the Example Plug-in. That's the whole point of the configuration being Haskell.

moveEmacs :: Move -> Actions ()
moveEmacs = \case

 Move Left_ Character -> press C b
 Move Right_ Character -> press C f
 Move Left_ Word_ -> press M b
 Move Right_ Word_ -> press M f
 Move Left_ Group -> press C M b
 Move Right_ Group -> press C M f
 Move Up_ Line -> press C p
 Move Down_ Line -> press C n
 Move Up_ Block -> press C up
 Move Down_ Block -> press C down
 Move Up_ Screen -> runEmacs "scroll-up-command"
 Move Down_ Screen -> press C v
 Move Up_ Page -> runEmacs "backward-page"
 Move Down_ Page -> runEmacs "forward-page"

 MoveTo Backwards Line -> press C a
 MoveTo Forwards  Line -> press C e
 MoveTo Backwards Everything -> press M up
 MoveTo Forwards Everything -> press M down

 -- Move -> press
 -- MoveTo -> press
 _ -> nothing




editEmacs :: Edit -> Actions ()
editEmacs = \case

 -- Select
 -- Beginning
 -- End
 -- Copy
 -- Cut
 -- Delete
 -- Transpose
 -- Google

 -- That
 -- Character
 -- Word_
 -- Token
 -- Group
 -- Line
 -- Rectangle
 -- Block
 -- Page
 -- Screen
 -- Everything
 -- Definition
 -- Function_
 -- Reference
 -- Structure

 _ -> nothing





nothing = return ()
slot s = do
 -- delay 30
 sendText s
 sendKeyPress [] ReturnKey

always = const
when :: [CompilerContext] -> Actions () -> (CompilerContext -> Actions ())
when theseContexts thisAction = \theContext -> do
 if theContext `List.elem` theseContexts
 then thisAction
 else nothing
onlyWhen = when . (:[])
whenEmacs = onlyWhen "emacs"

munge :: Phrase -> Actions String
munge p = do
 q <- splatPasted p <$> getClipboard
 return $ mungePhrase q defSpacing







-- |
--
-- 'Phrase_' is the unassociated concrete syntax list
-- (e.g. tokens, parentheses),
-- while 'Phrase' is the associated abstract syntax tree (e.g. s-expressions).
data Phrase_
 = Escaped_  Keyword -- ^ atom-like.
 | Quoted_   Dictation -- ^ list-like.
 | Pasted_ -- ^ atom-like.
 | Blank_ -- ^ atom-like.
 | Separated_ Separator -- ^ like a "close paren".
 | Cased_      Casing -- ^ function-like (/ "open paren").
 | Joined_     Joiner -- ^ function-like (/ "open paren").
 | Surrounded_ Brackets -- ^ function-like (/ "open paren").
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
 (\context -> try (pP context <?> showLHS l))

 where
 Just l = lhsFromName 'phrase
 dependencies = [] <$ liftGrammar (phraseA `eitherG` phraseB `eitherG` phraseC `eitherG` dictation)
 -- TODO the RHS of special grammars are ignored (so the eithers don't matter), except for extracting dependencies for serialization

 pP context                     -- merges the context-free Parsec.many the context-sensitive manyUntil
    = ([]    <$  (case context of Some q -> (try . lookAhead) q *> pure (error "pP"))) -- terminate.
  <|> ((:)   <$> pAB             <*> pP context)  -- continue. e.g. can escape "say" with "lit"
  <|> ((:[]) <$> pC context) -- terminate.
  <|> ((:)   <$> (pDxAB context) <*> pP context)  -- continue
 pAB = pA <|> pB
 pDxAB context = Dictated_ <$> pD (case context of Some q -> Some (pAB <|> (q *> pure (error "pDxAB"))))
 -- pAB context = (pA context <||> pB context)
 -- pD'AB context = ((Right . Dictated) <$> pD) `manyUntil` (pAB <|> context)
 pA         = try $ phraseA   ^. gramParser $ (error "pA") -- context free
 pB         = try $ phraseB   ^. gramParser $ (error "pB") -- context free
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
--
-- this ordering prioritizes the escaping Escaped_/Quoted_ over the
-- escaped, e.g. "quote greater equal unquote".
phraseA = 'phraseA <=> empty
 <|> Escaped_    # "lit" & keyword
 <|> Quoted_     # "quote" & dictation & "unquote"
 <|> Pasted_     # "paste"
 <|> Blank_      # "blank"
 <|> (Spelled_ . (:[])) # letter_
 <|> (Spelled_ . (:[])) # character
 <|> Separated_  # separator
 <|> Cased_      # casing
 <|> Joined_     # joiner
 <|> Surrounded_ # brackets
-- | a sub-phrase where a phrase to the right is possible.
phraseB = 'phraseB <=> empty
 -- TODO letters grammar that consumes tokens with multiple capital letters, as well as tokens with single aliases
 -- <|> Spelled_  # "spell" & letters -- only, not characters
 <|> Spelled_  # "spell" & (character-+)
 <|> Capped_   # "caps" & (character-+)
 -- <$> alphabetRHS
-- | a sub-phrase where a phrase to the right is impossible.
phraseC = 'phraseC <=> Dictated_ # "say" & dictation
-- TODO maybe consolidate phrases ABC into a phrase parser, with the same grammar, but which injects different constructors i.e. different views into the same type

newtype Separator = Separator String  deriving (Show,Eq,Ord)
separator = 'separator <=> empty
 <|> Separator ""  # "break" -- separation should depend on context i.e. blank between symbols, a space between words, space after a comma but not before it. i.e. the choice is delayed until munging.
 <|> Separator " " # "space"
 <|> Separator "," # "comma"



-- | used by 'pPhrase'.
--
--
type PStack = NonEmpty PItem
-- -- the Left represents 'List', the Right represents 'Sexp', 'Atom' is not represented.
-- type PStack = NonEmpty (Either [Phrase] (PFunc, [Phrase]))

-- | an inlined subset of 'Sexp'.
--
-- Nothing represents 'List', Just represents 'Sexp', 'Atom' is not represented.
type PItem = (Maybe PFunc, [Phrase])

joinSpelled :: [Phrase_] -> [Phrase_]
joinSpelled = foldr' go []
 where
 go :: Phrase_ -> [Phrase_] -> [Phrase_]
 go (Spelled_ xs) (Spelled_ ys : ps) = (Spelled_ $ xs <> ys) : ps
 go p ps = p:ps

-- | parses "tokens" into an "Sexp". a total function.
pPhrase :: [Phrase_] -> Phrase
pPhrase = fromStack . foldl' go ((Nothing, []) :| []) . joinSpelled
 -- (PSexp (PList [PAtom (PWord "")]))
 where
 go :: PStack -> Phrase_ -> PStack
 go ps = \case
  (Escaped_  (x))            -> update ps $ fromPAtom (PWord x)
  (Quoted_   (Dictation xs)) -> update ps $ List ((fromPAtom . PWord) <$> xs)
  (Dictated_ (Dictation xs)) -> update ps $ List ((fromPAtom . PWord) <$> xs)
  (Capped_   cs)             -> update ps $ fromPAtom (PAcronym cs)
  (Spelled_  cs)             -> update ps $ fromPAtom (PAcronym cs)
  Pasted_                    -> update ps $ fromPasted
  Blank_                     -> update ps $ fromPAtom (PWord "")
  Separated_ (Separator x) -> update (pop ps) $ fromPAtom (PWord x)
  -- Separated_ Broken -> update (pop ps)
  (Cased_     f)  -> push ps (Cased f)
  (Joined_    f)  -> push ps (Joined f)
  (Surrounded_ f) -> push ps (Surrounded f)

 pop :: PStack -> PStack
 -- break from the innermost PFunc, it becomes an argument to the outer PFunc
 -- i.e. close the S expression with a right parenthesis "...)"
 pop ((Nothing,ps):|(q:qs)) = update (q:|qs) (List ps)
 pop ((Just f ,ps):|(q:qs)) = update (q:|qs) (Sexp f ps)
 -- if too many breaks, just ignore
 pop stack = stack
 -- i.e. open a left parenthesis with some function "(f ..."
 push :: PStack -> PFunc -> PStack
 push (p:|ps) f = (Just f, []) :| (p:ps)

 update :: PStack -> Phrase -> PStack
 update ((f,ps):|qs) p = (f, ps <> [p]) :| qs

 -- right-associate the PFunc's.
 fromStack :: PStack -> Phrase
 fromStack = fromItem . foldr1 associateItem . NonEmpty.reverse

 associateItem :: PItem -> PItem -> PItem
 associateItem (f,ps) = \case
  (Nothing,qs) -> (f, ps <> [List   qs])
  (Just g ,qs) -> (f, ps <> [Sexp g qs])

 fromItem :: PItem -> Phrase
 fromItem (Nothing, ps) = List   ps
 fromItem (Just f,  ps) = Sexp f ps

 fromPasted :: Phrase
 fromPasted = Atom . Left $ Pasted

 fromPAtom :: PAtom -> Phrase
 fromPAtom = Atom . Right





data Move
 = Move Direction Region
 | MoveTo Slice Region
 deriving (Show,Eq,Ord)
move = 'move
 <=> Move   # direction & region
 <|> MoveTo # slice & region
-- boilerplate.
-- can't scrap it with GHC.generics because the grammars are values not instance methods.
-- we could scrap it with TemplateHaskell if we were really wanted to, to gain that edit-once property, lowercasing the type to get the value, but I don't want to.


data Direction = Up_ | Down_ | Left_ | Right_ | In_ | Out_  deriving (Show,Eq,Ord,Enum,Typeable)
direction = tidyGrammar
-- direction = transformedGrammar (filter (/= '_'))
-- direction = qualifiedGrammarWith "_"



data Edit = Edit Action Slice Region deriving (Show,Eq,Ord)
edit = 'edit <=> empty
 <|> Edit # action & (slice -?- Whole) & region
 -- this causes the errors in parsing "say 638 Pine St., Redwood City 94063":
 -- <|> editing # (action-?) & (slice-?) & (region-?)
 -- probably because it always succeeds, because [zero*zero*zero = zero] i.e.
 -- I don't know why the alternatives following the black hole didn't show up in the "expecting: ..." error though
-- TODO ensure no alternative is empty, necessary? yes it is

-- TODO This should be exposed as a configuration. editConfig? editWith defEditing?
-- editWith editing = 'edit <=> editing # (direction-?) & (action-?) & (region-?)
-- edit = editWith defEditing
-- TODO
-- maybe RHS should have access to a configuration environment? Oh my.
-- could also provide the keyword (i.e. only literals) feature, rather than forcing it on the parser.
-- if it were State not Reader, it could also support contextual (mutable) vocabularies;
 -- no, that makes the code to hard to read I think. The controller should handle the mutation/reloading, not the model.
editing :: Maybe Action -> Maybe Slice -> Maybe Region -> Edit
editing = undefined
-- TODO defaults <|> Edit # action & region
 -- <|> Edit undefined # region
 -- <|> flip Edit undefined # action

data Slice = Backwards | Whole | Forwards  deriving (Show,Eq,Ord,Enum,Typeable)
-- data Slice = BackSlice | WholeSlice | ForSlice deriving (Show,Eq,Ord,Enum,Typeable)
-- slice = qualifiedGrammar
slice = 'slice
 <=> Backwards # "back"
 <|> Whole     # "whole"
 <|> Forwards  # "for"

-- "for" is homophone with "four".
-- and both Positive and Slice can be the prefix (i.e. competing for the same recognition).




data Action
 = Select
 | Beginning
 | End
 | Copy
 | Cut
 | Delete
 | Transpose
 | Google
 deriving (Show,Eq,Ord,Typeable)
-- action = enumGrammar
action = 'action <=> empty
 <|> Select      # "sell"
 <|> Beginning   # "beg"
 <|> End         # "end"
 <|> Copy        # "save"
 <|> Cut         # "kill"
 <|> Delete      # "del"
 <|> Transpose   # "trans"
 <|> Google      # "google"



data Region
 = That
 | Character
 | Word_
 | Token
 | Group
 | Line
 | Rectangle
 | Block
 | Page
 | Screen
 | Everything
 | Definition
 | Function_
 | Reference
 | Structure
 deriving (Show,Eq,Ord,Enum,Typeable)
-- region = enumGrammar
region = 'region
 <=> That       # "that"
 <|> Character  # "char"
 <|> Word_      # "word"
 <|> Token      # "toke"
 <|> Group      # "group"
 <|> Line       # "line"
 <|> Rectangle  # "wreck"
 <|> Block      # "block"
 <|> Page       # "page"
 <|> Screen     # "screen"
 <|> Everything # "all"
 <|> Definition # "def"
 <|> Function_  # "fun"
 <|> Reference  # "ref"
 <|> Structure  # "struct"









casing = enumGrammar
joiner = 'joiner
 <=> (\c -> Joiner [c]) # "join" & character
 <|> Joiner "_" # "snake"
 <|> Joiner "-" # "dash"
 <|> Joiner "/" # "file"
 <|> Joiner ""  # "squeeze"
 <|> CamelJoiner # "camel"
 <|> ClassJoiner # "class"
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

-- | 'Key's and 'Char'acters are "incomparable sets":
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
  Right raw_p -> do
   let pasted_p   = pPhrase raw_p
   let splatted_p = splatPasted pasted_p ("clipboard contents")
   let munged_p   = mungePhrase splatted_p defSpacing
   print $ raw_p
   print $ pasted_p
   print $ splatted_p
   print $ munged_p

attemptParse grammar s = do
 putStrLn ""
 attempt $ handleParse grammar s

failingParse grammar s = do
 putStrLn ""
 attempt $ case grammar `parses` s of
  Left e  -> do
   putStrLn "should fail, and it did:"
   putStrLn $ "error = " <> show e
  Right a -> do
   putStrLn "should fail, but succeeded:"
   putStrLn $ "input  = " <> show s
   putStrLn $ "output = " <> show a

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

attemptInterpret = ()



main = do

 putStrLn ""
 let rootG = (root^.comGrammar)
 attemptSerialize rootG
 -- attemptSerialize phrase

 putStrLn ""
 attemptParse phraseC "say 638 Pine St., Redwood City 94063"

 putStrLn ""
 -- Error (line 1, column 1): unexpected 's'
 -- expecting positive__Commands.Plugins.Example__commands-core-0.0.0, "replace", "no", "no way", click__Commands.Plugins.Example__commands-core-0.0.0, edit__Commands.Plugins.Example__commands-core-0.0.0 or end of input
 attemptParse rootG "say 638 Pine St., Redwood City 94063"
 -- when we remove the alternatives which are listed in the error above:
 -- Phrase_ (List [List [Atom (Right (PWord "638")),Atom (Right (PWord "Pine")),Atom (Right (PWord "St.,")),Atom (Right (PWord "Redwood")),Atom (Right (PWord "City")),Atom (Right (PWord "94063"))]])

 -- prefix succeeds, but the whole should fail
 -- should it fail? If it backtracks sufficiently, the wildcard (dictation in phrase, a later alternative) can match it
 -- Otherwise, any prefix must be escaped (e.g. by "lit")
 failingParse rootG "no bad"
 -- when we remove the alternatives which are listed in the error above:
 -- should fail, but succeeded:
 -- "input  = \"no BAD\""
 -- "output = Phrase_ (List [List [Atom (Right (PWord \"no\"))],Atom (Right (PAcronym \"BAD\"))])"

 attemptParse (multipleG rootG) "no no 1 replace this and that with that and this"
 attemptParse click "click"
 -- attemptParse directions "directions from Redwood City to San Francisco by public transit"
 print $ getWords (rootG ^. gramGrammar)


--  , ""
 putStrLn ""
 traverse_ attemptMunge_
  [ "coal server space tick local"  -- :server 'local --
-- "curly spaced coal server tick local coal key value"  -- {:server 'local :key value}
 -- where {spaced} means {| all isAlphaNum l && all isAlphaNum r -> " "} i.e. space out words always
  , "camel quote double great equals unquote space eek ace par great great eek"  -- doubleGreaterEquals = (>>=) -- "doubleGreaterSpacedEqualEquals(doublegreaterequal)" -- "\"Double>ErEqualUnquote   d equals (double>erequal)"
  -- , "camel quote double greater equal unquote spaced equals par double greater equal"  -- doubleGreaterEquals = (>>=) -- "doubleGreaterSpacedEqualEquals(doublegreaterequal)" -- "\"Double>ErEqualUnquote   d equals (double>erequal)"
  , "class unit test spell M T A"  -- UnitTestMTA
  , "camel M T A bid optimization"  -- mtaBidOptimization -- "mTABidOptimization"
  , "class spell M T A bid optimization"  -- MTABidOptimization
  , "spell M T A class bid optimization"  -- MTABidOptimization -- "mta BidOptimization"
  , "class M T A bid optimization"  -- MTABidOptimization
  , "class spell M TA bid optimization"  -- MTABidOptimization
  , "lit say camel say some words"  -- say someWords
  , "upper paste"
  , "camel paste" -- "clipboard contents"
  , "class paste" -- "clipboard contents"
  , "lore grave camel with async grave space action roar"  -- (`withAsync` action) -- "lore grave withAsyncGraveSpaceActionRoar"
  , "par round grave camel with async break break action"  -- (`withAsync`action) -- "(`withAsync`action)"
  , "par round grave camel with async break space action"  -- (`withAsync` action) -- "(`withAsync`action)"
  ]  -- TODO "spaced" only modifies the one token to the right, unlike the other joiners which modify all tokens to the right


 putStrLn ""
 traverse_ (attemptParse $ root^.comGrammar)
  [ "no"
  , "replace this and that with that and this"  -- "this and that" -> "that and this"
  , "replace paste with blank"                  --  ~ delete the clipboard between here and the end of the buffer
  , "replace par round grave camel lit with async break break action with blank"  -- "(`withAsync` action)" -> ""
  ]

 putStrLn ""
 attemptCompile root "emacs" "replace clipboard contents with paste"

 attemptParse move  "back word"
 attemptParse move  "up line"
 attemptParse rootG "back word"
 attemptParse rootG "up line"

 -- the keys are in the right order, and multiple modifiers applied to each
 putStrLn $ showActions $ press C M tab ZKey 'O' "abc" 1 (-123)

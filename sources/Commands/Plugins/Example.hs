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
root :: C Root
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
   "emacs" -> runEmacsWithP "replace-regexp" [this, that]
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

always = const
when :: [Application] -> Actions () -> (Application -> Actions ())
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

nothing = return ()

slot s = do
 delay 10
 sendText s
 sendKeyPress [] ReturnKey

execute_extended_command = press C w -- non-standard: make this configurable? ImplicitParams?

eval_expression = press M ':'

-- | as it opens a minibuffer, it needs @(setq enable-recursive-minibuffers t)@ to work when already in a minibuffer.
evalEmacs :: ElispSexp -> Actions ()
evalEmacs sexp = do
 eval_expression
 slot sexp

type ElispSexp = String
-- -- type ElispSexp = Sexp String String

-- parseSexp :: String -> Possibly ElispSexp
-- parseSexp = undefined

-- prettySexp :: ElispSexp -> String
-- prettySexp = undefined

runEmacs :: String -> Actions ()
runEmacs f = runEmacsWith f []

{- | pseudo-rpc:

* "rpc" because you can call emacs commands (I.e. interactive functions) with arguments

* "pseudo" because the return type is unit: the communication is via one-way keyboard-shortcuts, rather than a two-way channel like a network connection.

no "type"-checking or ararity-checking.

as it opens a minibuffer, it needs @(setq enable-recursive-minibuffers t)@ to work when already in a minibuffer.

-}
runEmacsWith :: String -> [String] -> Actions ()
runEmacsWith f xs = do
 execute_extended_command -- non-standard: make this configurable? ImplicitParams?
 slot f
 traverse_ slot xs
-- configurable by actions being a ReaderT? The solved another problem, delays or something.
-- or just let the user define it, after copying and pasting the Example Plug-in. That's the whole point of the configuration being Haskell.
--
-- integrate with a vocabulary. or simple sum grammar, falling back to dictation.
-- print a list of all interactive commands, tokenize by splitting on "-".
-- http://stackoverflow.com/questions/29953266/emacs-list-the-names-of-every-interactive-command

runEmacsWithA :: String -> [Actions String] -> Actions ()
runEmacsWithA f as = do
 xs <- traverse id as
 runEmacsWith f xs

runEmacsWithP :: String -> [Phrase] -> Actions ()
runEmacsWithP f ps = do
 xs <- traverse munge ps
 runEmacsWith f xs

moveEmacs :: Move -> Actions ()
moveEmacs = \case

 Move Left_ Character  -> press C b
 Move Right_ Character -> press C f
 Move Left_ Word_      -> press M b
 Move Right_ Word_     -> press M f
 Move Left_ Group      -> press C M b
 Move Right_ Group     -> press C M f
 Move Up_ Line         -> press C p
 Move Down_ Line       -> press C n
 Move Up_ Block        -> press C up
 Move Down_ Block      -> press C down
 Move Up_ Screen       -> runEmacs "scroll-up-command"
 Move Down_ Screen     -> press C v
 Move Up_ Page         -> runEmacs "backward-page"
 Move Down_ Page       -> runEmacs "forward-page"

 MoveTo Beginning Line       -> press C a
 MoveTo Ending  Line       -> press C e
 MoveTo Beginning Everything -> press M up
 MoveTo Ending Everything  -> press M down

 -- Move -> press
 -- MoveTo -> press
 _ -> nothing

-- TODO read application from environment, which determines the keyboard shortcut
-- an application is defined by the keyboard shortcuts it supports?
-- Rec?
-- Map String Actions
-- lookup "mark"
mark = press C spc

-- gets the given region of text from Emacs
selected :: Slice -> Region -> Actions String
selected s r = do
 -- editEmacs (Edit Select s r)
 select r s
 copy

select :: Region -> Slice -> Actions ()
select That = \_ -> nothing     -- (should be) already selected
select r = \case
 Whole     -> beg_of r >> mark >> end_of r
 Backwards -> mark >> beg_of r
 Forwards  -> mark >> end_of r

{-

idempotent means

idempotent means unchainable.
instead of [3 select word], how about [select 3 word]
where the first selection is idempotent, and the next two Move Right.
In Emacs, this preserves the mark.



-}
-- | should be idempotent (in Emacs, not Haskell).
beg_of :: Region -> Actions ()
beg_of = \case
 -- runEmacs
 That       -> evalEmacs "(goto-char (region-beginning))"
 Character  -> nothing
 Word_      -> evalEmacs "(beginning-of-thing 'word)"
 Group      -> evalEmacs "(beginning-of-thing 'list)"
 Line       -> press C a
 Block      -> evalEmacs "(beginning-of-thing 'block)"
 Page       -> evalEmacs "(beginning-of-thing 'page)"
 Screen     -> evalEmacs "(goto-char (window-start))"
 Everything -> runEmacs "beginning-of-buffer"
 _          -> nothing

-- | should be idempotent (in Emacs, not Haskell).
end_of :: Region -> Actions ()
end_of = \case
 -- runEmacs
 That       -> evalEmacs "(goto-char (region-end))"
 Character  -> nothing          -- [press C f] is not idempotent, but [nothing] fails on [beg_of r >> mark >> end_of r]
 Word_      -> evalEmacs "(end-of-thing 'word)"
 Group      -> evalEmacs "(end-of-thing 'list)"
 Line       -> press C e
 Block      -> evalEmacs "(end-of-thing 'block)" -- non-standard: expects forward-block
 Page       -> evalEmacs "(end-of-thing 'page)"
 Screen     -> evalEmacs "(goto-char (window-end))"
 Everything -> runEmacs "end-of-buffer"
 _          -> nothing

-- | vim's composeability would keep the number of cases linear (not quadratic in 'Action's times 'Region's).
-- in Emacs, we can use <http://www.emacswiki.org/emacs/ThingAtPoint thingatpt.el>.
editEmacs :: Edit -> Actions ()
editEmacs = \case

 Edit Select Whole Line -> do -- special behavior
  select Line Whole
  press right
 Edit Select _ Character -> do -- special behavior
  mark
  press right
 Edit Select s r -> select r s  -- generic behavior
 -- some Regions need a { press right } for idempotency of their beg_of/end_of

 Edit Google s r -> do
  google =<< selected s r

 Edit Delete s Word_ -> do
  select Word_ s
  press right                   -- doesn't work for camel case. only single-character-delimited. maybe Token, not Word?
  press del
 Edit Delete s r -> do
  select r s
  press del

 Edit Copy s r -> do
  select r s
  press M c                     -- like Cua-mode for Mac

 Edit Cut s r -> do
  select r s
  press M x                     -- like Cua-mode for Mac

 Edit Transpose _ Character -> press C t
 Edit Transpose _ Word_ -> press M t
 Edit Transpose _ Group -> press C M t
 Edit Transpose _ Line -> press C x t
 Edit Transpose _ Block -> runEmacs "transpose-block" -- nonstandard
 -- Edit Transpose _ ->

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



data Move
 = Move Direction Region
 | MoveTo Endpoint Region
 deriving (Show,Eq,Ord)
move = 'move
 <=> Move   # direction & region
 <|> MoveTo # endpoint & region
-- boilerplate.
-- can't scrap it with GHC.generics because the grammars are values not instance methods.
-- we could scrap it with TemplateHaskell if we were really wanted to, to gain that edit-once property, lowercasing the type to get the value, but I don't want to.

-- | Slice and Direction both have too many values.
data Endpoint = Beginning | Ending deriving (Bounded,Enum,Eq,Ord,Read,Show)
endpoint = 'endpoint
 <=> Beginning # "beg"
 <|> Ending    # "end"

-- | orthogonal directions in three-dimensional space.
data Direction = Up_ | Down_ | Left_ | Right_ | In_ | Out_  deriving (Show,Eq,Ord,Enum,Typeable)
direction = tidyGrammar
-- direction = transformedGrammar (filter (/= '_'))
-- direction = qualifiedGrammarWith "_"

{- | slice the region between the cursor and the 'Slice'. induces a string.
-}
data Slice = Whole | Backwards | Forwards  deriving (Show,Eq,Ord,Enum,Typeable)
-- data Slice = BackSlice | WholeSlice | ForSlice deriving (Show,Eq,Ord,Enum,Typeable)
-- slice = qualifiedGrammar
slice = 'slice
 <=> Whole     # "whole"
 <|> Backwards # "back"
 <|> Forwards  # "for"

-- "for" is homophone with "four", while both Positive and Slice can be the prefix (i.e. competing for the same recognition).



data Edit = Edit Action Slice Region deriving (Show,Eq,Ord)
edit = 'edit <=> empty
 -- aliases: constructors are more specific (e.g. @Edit Cut Forwards Line@) than later alternatives; 'RHS's are prefixes (or identical) to later alternatives (e.g. @# "kill"@)
 -- prefixes (e.g. "kill") must come before their superstrings (e.g. "kill for line").
 -- otherwise, the prefix is committed prematurely, and parsec won't backtrack.
 -- TODO but wouldn't @# action & (slice -?- Whole) & (region -?- That)@ match "kill" before @# "kill"@ does? yes it does

 -- TODO we want:
 -- "cop" -> Edit Copy Whole That
 -- "kill" -> Edit Cut Forwards Line, not Edit Cut Whole That
 -- "kill for line" -> Edit Cut Forwards Line, not {unexpected 'f', expecting end of input}
 <|> Edit Cut Forwards Line # "kill"

 -- generic
 <|> Edit # action              & (slice -?- Whole) & (region -?- That) -- e.g. "cop" -> "cop that"
 <|> Edit # (action -?- Select) & (slice -?- Whole) & region            -- e.g. "word" -> "select whole word"

-- TODO ensure no alternative is empty, necessary? yes it is
 -- this causes the errors in parsing "say 638 Pine St., Redwood City 94063":
 -- <|> editing # (action-?) & (slice-?) & (region-?)
 -- probably because it always succeeds, because [zero*zero*zero = zero] i.e.
 -- I don't know why the alternatives following the annihilator didn't show up in the "expecting: ..." error though

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




data Action
 = Select                       -- read-only.
 | Copy                         -- read-only.
 | Cut                          -- read/write.
 | Delete                       -- read/write.
 | Transpose                    -- read/write.
 | Google                       -- read-only.
 deriving (Show,Eq,Ord,Typeable)
-- action = enumGrammar
action = 'action <=> empty
 <|> Select      # "sell"
 <|> Copy        # "cop"
 <|> Cut         # "kill"
 <|> Delete      # "del"
 <|> Transpose   # "trans"
 <|> Google      # "google"



data Region
 = That

 | Character
 | Word_                        -- ^ e.g. @"camelCase"@, @"lisp-case"@, @"snake_case"@ are all two 'Word_'s
 | Token                        -- ^ e.g. @"camelCase"@, @"lisp-case"@, @"snake_case"@ are all one 'Token's
 | Group                        -- ^ 'Bracket's delimit 'Group's (e.g. @"(...)"@ or @"<...>"@ or @"[|...|]"@)
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


-- | 'Key's and 'Char'acters are "incomparable sets":
--
-- * many modifiers are keys that aren't characters (e.g. 'CommandKey')
-- * many nonprintable characters are not keys (e.g. @\'\\0\'@)
--
-- so we can't embed the one into the other, but we'll just keep things simple with duplication.
--
key = 'key <=> empty

data Click = Click Times Button deriving (Show,Eq)
click = 'click <=>
 Click # optionalEnum times & optionalEnum button & "click"
 -- type inference with the {&} sugar even works for:
 --  Click # optionalEnum enumGrammar & optionalEnum enumGrammar & "click"
 -- the terminal "click" makes the grammar "non-canonical" i.e.
 --  where product types are merged with <*> (after "lifting" into RHS)
 --  and sum types are merged with <|> (after "tagging" with the constructor)

data Times = Single | Double | Triple deriving (Show,Eq,Enum,Typeable)
times = enumGrammar :: G Times

data Button = LeftButton | MiddleButton | RightButton deriving (Show,Eq,Enum,Typeable)
button = qualifiedGrammar

positive :: G Positive
-- positive :: Grammar p r Positive
positive = 'positive
 <=> Positive <$> (asum . fmap int) [1..9]

{- the type annotation on positive was needed to disambiguate this type error, which I don't like:

TODO find out if this is necessary. I'd like positive to be defined generically.

sources/Commands/Plugins/Example.hs:531:15:
    No instance for (AppRHS
                       Parser DNSReifying (Alter (Symbol p0 r0) Int))
      arising from a use of ‘#’
    The type variables ‘p0’, ‘r0’ are ambiguous
    Note: there is a potential instance available:
      instance Functor p => AppRHS p r (RHS p r a)
        -- Defined in ‘Commands.Sugar’
    In the second argument of ‘(<=>)’, namely
      ‘Positive # (asum . fmap int) [1 .. 9]’
    In the expression:
      'positive <=> Positive # (asum . fmap int) [1 .. 9]
    In an equation for ‘positive’:
        positive = 'positive <=> Positive # (asum . fmap int) [1 .. 9]

sources/Commands/Plugins/Example.hs:531:18:
    No instance for (Functor p0) arising from a use of ‘asum’
    The type variable ‘p0’ is ambiguous
    Note: there are several potential instances:
      instance Functor m =>
               Functor (MonadRandom-0.3.0.2:Control.Monad.Random.RandT g m)
        -- Defined in ‘MonadRandom-0.3.0.2:Control.Monad.Random’
      instance Functor p =>
               Functor (Control.Applicative.Permutation.Branch p)
        -- Defined in ‘Control.Applicative.Permutation’
      instance Functor p => Functor (Perms p)
        -- Defined in ‘Control.Applicative.Permutation’
      ...plus 96 others
    In the first argument of ‘(.)’, namely ‘asum’
    In the expression: asum . fmap int
    In the second argument of ‘(#)’, namely
      ‘(asum . fmap int) [1 .. 9]’

-}










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
 attempt $ handleParse (grammar ^. gramParser) s

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
 putStrLn $ showActions $ editEmacs (Edit Google Whole Line)
 -- runActions $ google "some words" -- it works

 putStrLn $ showActions $ editEmacs (Edit Google Whole Line)
 putStrLn $ showActions $ editEmacs (Edit Cut Backwards Word_)

 attemptParse edit "cop"
 attemptParse edit "word"

 attemptParse rootG "kill for line" --
 attemptParse edit "kill for line" --
 attemptParse edit "kill"


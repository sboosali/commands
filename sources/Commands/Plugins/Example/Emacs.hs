{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable, DeriveFunctor     #-}
{-# LANGUAGE ExtendedDefaultRules, KindSignatures, LambdaCase          #-}
{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, PatternSynonyms #-}
{-# LANGUAGE PostfixOperators, RankNTypes, ScopedTypeVariables         #-}
{-# LANGUAGE StandaloneDeriving, TupleSections, TypeFamilies           #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-imports -fno-warn-type-defaults -fno-warn-partial-type-signatures #-}
module Commands.Plugins.Example.Emacs where
import           Commands.RHS.Types
import qualified Data.HRefCache.Internal as HRefCache
-- import Commands.Plugins.Example.Phrase hiding  (phrase, casing, separator, joiner, brackets, keyword, char, dictation, word)

import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.List.NonEmpty      as NonEmpty
import qualified Text.Earley             as E
import qualified Text.Earley.Grammar     as E
import qualified Text.Earley.Internal    as E

import           Control.Applicative
import           Control.Arrow           ((>>>))
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.Functor.Product
import           Data.IORef
import           Data.STRef


type RHSEarleyDNS r = RHS (ConstName String) String (EarleyF r String String [])

type EarleyF r n t f = (Product (E.Prod r n t) f)
-- type EarleyF a n t f = (forall r. (Product (E.Prod r n t) f))

(<=>) :: String -> RHSEarleyDNS r a -> RHSEarleyDNS r a
-- type signature for type inference: "No instance for (Data.String.IsString _)" and "No instance for (Functor _)"
(<=>) n r = NonTerminal (ConstName n) r
infix 2 <=>

liftEarley p = liftRHS (Pair p [])

anyWord = E.Terminal (const True) (pure id)

-- edits :: RHSEarleyDNS r (NonEmpty Edit)
-- edits :: RHS (ConstName String) String (forall r. (EarleyF r (NonEmpty Edit) String String [])) (NonEmpty Edit)
-- edits :: RHS (ConstName String) String (EarleyF r a String String f) (NonEmpty Edit)
-- edits :: RHS (ConstName String) String [] (NonEmpty Edit)
-- edits :: RHS (ConstName String) String _ (NonEmpty Edit)
-- edits :: Functor f => RHS (ConstName String) String f (NonEmpty Edit)
edits = "edits" <=> (edit-+)

data Edit = Edit Action Slice Region deriving (Show,Eq,Ord)
-- edit :: RHSEarleyDNS r Edit
edit = "edit" <=> empty
 <|> Edit Cut Forwards Line <$ "kill"
 <|> Edit <$> action            <*> (slice-?-Whole) <*> (region-?-That)
 <|> Edit <$> (action-?-Select) <*> (slice-?-Whole) <*> region

data Action
 = Select                       -- read-only.
 | Copy                         -- read-only.
 | Cut                          -- read/write.
 | Delete                       -- read/write.
 | Transpose                    -- read/write.
 | Google                       -- read-only.
 deriving (Show,Eq,Ord)
-- action :: RHSEarleyDNS r Action
action = "action" <=> empty
 <|> Select      <$ "sell"
 <|> Copy        <$ "cop"
 <|> Cut         <$ "kill"
 <|> Delete      <$ "del"
 <|> Transpose   <$ "trans"
 <|> Google      <$ "google"

data Slice = Whole | Backwards | Forwards  deriving (Show,Eq,Ord,Enum)
-- slice :: RHSEarleyDNS r Slice
slice = "slice"
 <=> Whole     <$ "whole"
 <|> Backwards <$ "back"
 <|> Forwards  <$ "for"

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
 deriving (Show,Eq,Ord,Enum)
-- region :: RHSEarleyDNS r Region
region = "region"
 <=> That       <$ "that"
 <|> Character  <$ "char"
 <|> Word_      <$ "word"
 <|> Token      <$ "toke"
 <|> Group      <$ "group"
 <|> Line       <$ "line"
 <|> Rectangle  <$ "wreck"
 <|> Block      <$ "block"
 <|> Page       <$ "page"
 <|> Screen     <$ "screen"
 <|> Everything <$ "all"
 <|> Definition <$ "def"
 <|> Function_  <$ "fun"
 <|> Reference  <$ "ref"
 <|> Structure  <$ "struct"

data Phrase
 -- continuation necessary
 = Case             Casing                       Phrase
 | Join             Joiner                       Phrase
 | Surround         Brackets                     Phrase
 | Separated        Separator                    Phrase
 | Spelled          [Char]                       Phrase
 | Letter           Char                         Phrase
 | Cap              Char                         Phrase
 | Pasted                                        Phrase
 | Blank                                         Phrase
 -- continuation optional
 | Escaped     Keyword                    (Maybe Phrase)
 | Quoted      Dictation                  (Maybe Phrase)
 | Dictated    Dictation                  (Maybe Phrase)
 -- continuation prohibited
 | Dictation_ Dictation
 deriving (Show,Eq,Ord)

data Casing = Upper | Lower | Capper deriving (Show,Eq,Ord,Enum)
data Joiner = Joiner String | CamelJoiner | ClassJoiner deriving (Show,Eq,Ord)
data Brackets = Brackets String String deriving (Show,Eq,Ord)
newtype Separator = Separator String  deriving (Show,Eq,Ord)
newtype Keyword = Keyword String deriving (Show,Eq,Ord)
newtype Dictation = Dictation [String] deriving (Show,Eq,Ord)

bracket :: Char -> Brackets
bracket c = Brackets [c] [c]



phrase = "phrase"
  -- continuation necessary
  <=> Case       <$>              casing <*>                              phrase
  <|> Join       <$>              joiner <*>                              phrase
  <|> Surround   <$>            brackets <*>                              phrase
  <|> Separated  <$>           separator <*>                              phrase
  <|> Spelled    <$ "spell"              <*> (char-++) <*>                 phrase
  <|> Letter     <$>                char <*>                              phrase
  <|> Cap        <$ "cap"                <*>     char <*>                 phrase
  <|> Pasted     <$ "paste"              <*>                              phrase
  <|> Blank      <$ "blank"              <*>                              phrase
  -- continuation optional
  <|> Escaped    <$ "lit"                <*>   keyword <*>               ( phrase-?)
  <|> Quoted     <$ "quote"              <*>   word    <*> ("unquote" *> ( phrase-?))
  <|> Dictated   <$>                word <*>                             ( phrase-?)
  -- continuation prohibited
  <|> Dictation_ <$> word

separator = "separator"
  <=> Separator ""  <$ "break"
  <|> Separator " " <$ "space"
  <|> Separator "," <$ "comma"
  <|> Separator "/" <$ "slash"
  <|> Separator "." <$ "dot"

casing = "casing"
  <=> Upper  <$ "upper"
  <|> Lower  <$ "lower"
  <|> Capper <$ "capper"

joiner = "joiner"
  <=> (\c -> Joiner [c]) <$ "join" <*> char
  <|> Joiner "_"  <$ "snake"
  <|> Joiner "-"  <$ "dash"
  <|> Joiner "/"  <$ "file"
  <|> Joiner ""   <$ "squeeze"
  <|> CamelJoiner <$ "camel"
  <|> ClassJoiner <$ "class"

brackets = "brackets"
  <=> bracket          <$ "round" <*> char
  <|> Brackets "(" ")" <$ "par"
  <|> Brackets "[" "]" <$ "square"
  <|> Brackets "{" "}" <$ "curl"
  <|> Brackets "<" ">" <$ "angle"
  <|> bracket '"'      <$ "string"
  <|> bracket '\''     <$ "ticked"
  <|> bracket '|'      <$ "norm"

keyword = "keyword"
  <=> Keyword <$> liftEarley anyWord

char = "char"
  <=> '`' <$ "grave"

dictation = "dictation"
  <=> Dictation <$> liftEarley (some anyWord)

word = "word"
  <=> (Dictation . (:[])) <$> liftEarley anyWord



renameRHSST
 :: (forall x. RHS n t f x -> n t f x -> ST s (n' t f x))
 -> ST s (RHS n t f a -> ST s (RHS n' t f a))
-- renameRHSST = undefined
-- renameRHSST u = do
--  c <- HRefCache.newCache
--  return$ renameRHS$ \t x -> do
--   k <- HRefCache.forceStableName t
--   readSTRef c >>= (HRefCache.lookupRef k >>> traverse readSTRef) >>= \case
--    Just y  -> return y
--    Nothing -> do
--     y <- u t x
--     v <- newSTRef y
--     _ <- modifySTRef' c ((,()) . HRefCache.insertRef k v)
--     return y
renameRHSST u = unsafeIOToST$ do
 c <- HRefCache.newCache
 return$ renameRHS$ \t x -> unsafeIOToST$ do
  k <- HRefCache.forceStableName t
  readIORef c >>= (HRefCache.lookupRef k >>> traverse readIORef) >>= \case
   Just y  -> return y          -- cache hit
   Nothing -> do                -- cache miss
    y <- unsafeSTToIO$ u t x
    v <- newIORef y
    _ <- atomicModifyIORef' c ((,()) . HRefCache.insertRef k v)
    return y
-- renaming recursive RHS doesn't terminate because: in renaming/traversing the non-terminal, the name is decoupled from the body. the natural transformation should return a pair, and the name and the rhs are both cached. makes sense, as we are caching on the non-terminal (isomorphic to a pair) pointer.

data EarleyName z n t (f :: * -> *) a = EarleyName
 { unEarleyName :: E.Prod z n t a -> E.Prod z n t a
 }
-- not a Functor

deriveEarley
 :: forall s r n t f a. ()
 => ST s (        RHS (ConstName                 n) t f a
         -> ST s (RHS (EarleyName (E.Rule s r) n) t f a)
         )
deriveEarley = renameRHSST $ \_ (ConstName n) -> do
 conts <- newSTRef =<< newSTRef []
 null  <- newSTRef Nothing
 return$ EarleyName (\p -> E.NonTerminal (E.Rule p null conts) (E.Pure id) E.<?> n)

induceEarley
 :: forall s r n t f a. (Eq t)
 => RHS (EarleyName (E.Rule s r) n) t (Product (E.Prod (E.Rule s r) n t) f) a
 -> E.Prod (E.Rule s r) n t a
induceEarley = runRHS
 (\n r -> (unEarleyName n) (induceEarley r))
 E.symbol
 (\(Pair p _) -> p)

buildEarley
 :: E.Prod (E.Rule s a) n t a
 -> [t]
 -> ST s (E.Result s n [t] a)
buildEarley p xs = do
  s <- E.initialState p
  E.parse [s] [] [] (return ()) [] 0 xs

runEarley
 :: (Eq t)
 => (forall r. RHS (ConstName n) t (Product (E.Prod (E.Rule s r) n t) f) a)
 -> [t]
 -> ST s (E.Result s n [t] a)
runEarley r1 xs = do
 r2 <- deriveEarley >>= ($ r1)
 buildEarley (induceEarley r2) xs

parse
 :: (Eq t)
 => (forall r. RHS (ConstName n) t (Product (E.Prod r n t) f) a)
 -> [t]
 -> ([a], E.Report n [t])
parse r xs = E.fullParses$ runEarley r xs

parseString
 :: (forall r. RHS (ConstName n) String (Product (E.Prod r n String) f) a)
 -> String
 -> [a]
parseString r s = as
 where (as,_) = E.fullParses$ runEarley r (words s)

mainEmacs = do
 print$ parseString edits "kill"
 print$ parseString edits "kill whole word"
 -- [Edit Cut Forwards Line :| [Edit Select Whole Word_]  "kill" and "_ whole word"
 -- ,Edit Cut Whole That :| [Edit Select Whole Word_]  "kill _ _" and "_ whole word"
 -- ,Edit Cut Whole That :| [Edit Select Whole Word_]  ?
 -- ,Edit Cut Whole Word_ :| []
 -- ,Edit Cut Whole Word_ :| []
 -- ]                                          --
 print$ parseString edit "kill"            -- [Edit Cut Forwards Line,Edit Cut Whole That] the correct order
 print$ parseString edit "kill whole word" -- [Edit Cut Whole Word_,Edit Cut Whole Word_] duplicate
 -- print$ parseString phrase "par round grave camel lit async break break action"
 -- print$ parseString root "replace par round grave camel lit with async break break action with blank"
 -- _phrase <- unsafeSTToIO (deriveEarley >>= ($ phrase))
 putStrLn ""

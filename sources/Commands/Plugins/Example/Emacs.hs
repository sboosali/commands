{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable, DeriveFunctor     #-}
{-# LANGUAGE ExistentialQuantification, ExtendedDefaultRules           #-}
{-# LANGUAGE FlexibleContexts, KindSignatures, LambdaCase              #-}
{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, PatternSynonyms #-}
{-# LANGUAGE PostfixOperators, RankNTypes, ScopedTypeVariables         #-}
{-# LANGUAGE StandaloneDeriving, TupleSections, TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances, EmptyCase, RecursiveDo, OverloadedLists , OverloadedStrings                                      #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-imports -fno-warn-type-defaults -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Example.Emacs where
import           Commands.Etc
import           Commands.RHS.Types
import qualified Data.HRefCache.Internal         as HRefCache
-- import Commands.Plugins.Example.Phrase hiding  (phrase, casing, separator, joiner, brackets, keyword, char, dictation, word)
import           Commands.Plugins.Example.Phrase (Phrase_ (..))
import qualified Commands.Plugins.Example.Phrase as P
import Commands.Frontends.Dragon13
import Data.Sexp

import Data.Void
import Data.Bifunctor
import Data.Bitraversable
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NonEmpty
import qualified Text.Earley                     as E
import qualified Text.Earley.Grammar             as E
import qualified Text.Earley.Internal            as E
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Text.Lazy (Text)
import Control.Lens hiding (snoc, (#))

import Control.Exception (Exception,SomeException (..))
import Data.Monoid              ((<>))
import           Control.Applicative
import           Control.Arrow                   ((>>>))
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.Foldable
import           Data.Function                   (on)
import           Data.Functor.Product
import           Data.IORef
import           Data.Ord                        (compare)
import           Data.STRef
import           Data.Unique
import Data.Functor.Classes
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans.State
-- import Control.Monad.Trans.Maybe
import Data.Functor.Foldable (Fix(..))
import Control.Comonad.Cofree
import qualified Data.List as List
import GHC.Exts (IsString(..))

default (Text) -- TODO Necessary? Sufficient?


-- ================================================================ --

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

 print$ parsePhrase "par round grave camel lit with async do break break action"
 -- "(`async`action)"
 print$ length $ parseString phrase_ "par round grave camel lit async break break action"

 -- loop print$ parseString phrase "par round grave camel lit async break break action"
 -- print$ parseString root "replace par round grave camel lit with async break break action with blank"
 -- _phrase <- unsafeSTToIO (renameRHSToEarley >>= ($ phrase))
 putStrLn ""
 T.putStrLn =<< showRHS phrase_

 print $ exampleSexpBlock


-- ================================================================ --

type DNSEarleyName n = ConstName (DNSInfo, n)

type DNSEarleyRHS z = RHS
 (DNSEarleyName String)
 Text
 (DNSEarleyFunc z (DNSEarleyName String) Text)

data DNSEarleyFunc z n t a
 =           LeafRHS (E.Prod z String t a) (DNSRHS t Void)
 | forall x. TreeRHS (RHS n t (DNSEarleyFunc z n t) a) (RHS n t (DNSEarleyFunc z n t) x)
-- couples parser (E.Prod) with format (DNSRHS) with (ConstName) :-(
deriving instance (Functor (n t (DNSEarleyFunc z n t))) => Functor (DNSEarleyFunc z n t) -- TODO UndecidableInstances
-- Variables ‘n, t’ occur more often than in the instance head in the constraint

data DNSFixName t = DNSFixName (DNSProduction DNSInfo (DNSFixName t) t)

(<=>) :: String -> DNSEarleyRHS z a -> DNSEarleyRHS z a
-- type signature for type inference, disambiguates:
--  "No instance for (Data.String.IsString _)" and "No instance for (Functor _)"
(<=>) n r = NonTerminal (ConstName (defaultDNSInfo, n)) r
infix 2 <=>

liftLeaf p r = liftRHS (LeafRHS p r)
liftTree p r = liftRHS (TreeRHS p r)

anyWord :: E.Prod z String Text Text
anyWord = E.Terminal (const True) (pure id)

_RHSInfo :: Traversal' (RHS (DNSEarleyName String) t f a) DNSInfo
_RHSInfo = _NonTerminal._1.unConstName._1


-- ================================================================ --

-- edits :: DNSEarleyRHS r (NonEmpty Edit)
-- edits :: RHS (ConstName String) String (forall r. (EarleyF r (NonEmpty Edit) String String [])) (NonEmpty Edit)
-- edits :: RHS (ConstName String) String (EarleyF r a String String f) (NonEmpty Edit)
-- edits :: RHS (ConstName String) String [] (NonEmpty Edit)
-- edits :: RHS (ConstName String) String _ (NonEmpty Edit)
-- edits :: Functor f => RHS (ConstName String) String f (NonEmpty Edit)
edits = "edits" <=> (edit-+)

data Edit = Edit Action Slice Region deriving (Show,Eq,Ord)
-- edit :: DNSEarleyRHS r Edit
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
-- action :: DNSEarleyRHS r Action
action = "action" <=> empty
 <|> Select      <$ "sell"
 <|> Copy        <$ "cop"
 <|> Cut         <$ "kill"
 <|> Delete      <$ "del"
 <|> Transpose   <$ "trans"
 <|> Google      <$ "google"

data Slice = Whole | Backwards | Forwards  deriving (Show,Eq,Ord,Enum)
-- slice :: DNSEarleyRHS r Slice
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
-- region :: DNSEarleyRHS r Region
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

-- data Phrase
--  -- continuation necessary
--  = Case             Casing                       Phrase
--  | Join             Joiner                       Phrase
--  | Surround         Brackets                     Phrase
--  | Separated        Separator                    Phrase
--  | Spelled          [Char]                       Phrase
--  | Letter           Char                         Phrase
--  | Cap              Char                         Phrase
--  | Pasted                                        Phrase
--  | Blank                                         Phrase
--  -- continuation optional
--  | Escaped     Keyword                    (Maybe Phrase)
--  | Quoted      Dictation                  (Maybe Phrase)
--  | Dictated    Dictation                  (Maybe Phrase)
--  -- continuation prohibited
--  | Dictation_ Dictation
--  deriving (Show,Eq,Ord)

-- data Casing = Upper | Lower | Capper deriving (Show,Eq,Ord,Enum)
-- data Joiner = Joiner String | CamelJoiner | ClassJoiner deriving (Show,Eq,Ord)
-- data Brackets = Brackets String String deriving (Show,Eq,Ord)
-- newtype Separator = Separator String  deriving (Show,Eq,Ord)
-- newtype Keyword = Keyword String deriving (Show,Eq,Ord)
-- newtype Dictation = Dictation [String] deriving (Show,Eq,Ord)

-- bracket :: Char -> Brackets
-- bracket c = Brackets [c] [c]



-- phrase = "phrase"
--   -- continuation necessary
--   <=> Case       <$>              casing <*>                              phrase
--   <|> Join       <$>              joiner <*>                              phrase
--   <|> Surround   <$>            brackets <*>                              phrase
--   <|> Separated  <$>           separator <*>                              phrase
--   <|> Spelled    <$ "spell"              <*> (char-++) <*>                phrase
--   <|> Letter     <$>                char <*>                              phrase
--   <|> Cap        <$ "cap"                <*>     char <*>                 phrase
--   <|> Pasted     <$ "paste"              <*>                              phrase
--   <|> Blank      <$ "blank"              <*>                              phrase
--   -- continuation optional
--   <|> Escaped    <$ "lit"                <*>   keyword <*>               ( phrase-?)
--   <|> Quoted     <$ "quote"              <*>   word    <*> ("unquote" *> ( phrase-?))
--   <|> Dictated   <$>                word <*>                             ( phrase-?)
--   -- continuation prohibited
--   <|> Dictation_ <$> word

-- separator = "separator"
--   <=> Separator ""  <$ "break"
--   <|> Separator " " <$ "space"
--   <|> Separator "," <$ "comma"
--   <|> Separator "/" <$ "slash"
--   <|> Separator "." <$ "dot"

-- casing = "casing"
--   <=> Upper  <$ "upper"
--   <|> Lower  <$ "lower"
--   <|> Capper <$ "capper"

-- joiner = "joiner"
--   <=> (\c -> Joiner [c]) <$ "join" <*> char
--   <|> Joiner "_"  <$ "snake"
--   <|> Joiner "-"  <$ "dash"
--   <|> Joiner "/"  <$ "file"
--   <|> Joiner ""   <$ "squeeze"
--   <|> CamelJoiner <$ "camel"
--   <|> ClassJoiner <$ "class"

-- brackets = "brackets"
--   <=> bracket          <$ "round" <*> char
--   <|> Brackets "(" ")" <$ "par"
--   <|> Brackets "[" "]" <$ "square"
--   <|> Brackets "{" "}" <$ "curl"
--   <|> Brackets "<" ">" <$ "angle"
--   <|> bracket '"'      <$ "string"
--   <|> bracket '\''     <$ "ticked"
--   <|> bracket '|'      <$ "norm"

char = "char"
  <=> '`' <$ "grave"

-- keyword = "keyword"
--   <=> Keyword <$> liftLeaf anyWord "keyword"

-- dictation = "dictation"
--   <=> Dictation <$> liftLeaf (some anyWord) "dictation"

-- word = "word"
--   <=> (Dictation . (:[])) <$> liftLeaf anyWord "word"



-- phrase_ = "phrase"
--  <=> snoc <$> ((phraseA <|> phraseB <|> phraseW)-*) <*> (phraseB <|> phraseC <|> phraseD)

phrase_ = "phrase" <=> liftTree
 (snoc <$> ((phraseA <|> phraseB <|> phraseW)-*) <*> (phraseB <|> phraseC <|> phraseD))
 (snoc <$> ((phraseA <|> phraseB <|> phraseD)-*) <*> (phraseB <|> phraseC <|> phraseD))

phraseA = "phraseA" <=> empty
 <|> Escaped_    <$ "lit" <*> keyword_
 <|> Quoted_     <$ "quote" <*> (dictation_ <* "unquote")
 <|> Pasted_     <$ "paste"
 <|> Blank_      <$ "blank"
 -- <|> (Spelled_ . (:[])) <$> char
 <|> Spelled_    <$ "spell" <*> (char-++)
 <|> Separated_  <$> separator_
 <|> Cased_      <$> casing_
 <|> Joined_     <$> joiner_
 <|> Surrounded_ <$> brackets_

phraseB = "phraseB" <=> empty
 -- TODO letters grammar that consumes tokens with multiple capital letters, as well as tokens with single aliases
 -- <|> Spelled_  <$> "spell" <*> letters -- only, not chars
 <|> Spelled_  <$ "spell" <*> (char-++)
 <|> Capped_   <$ "caps" <*> (char-++)
 -- <$> alphabetRHS

phraseC = "phraseC" <=> Dictated_ <$ "say" <*> dictation_

phraseW = "phraseW" <=> (Dictated_ . P.Dictation . (:[])) <$> word_

phraseD = "phraseD" <=> Dictated_ <$> dictation_

separator_ = "separator"
  <=> P.Separator ""  <$ "break"
  <|> P.Separator " " <$ "space"
  <|> P.Separator "," <$ "comma"
  <|> P.Separator "/" <$ "slash"
  <|> P.Separator "." <$ "dot"

casing_ = "casing"
  <=> P.Upper  <$ "upper"
  <|> P.Lower  <$ "lower"
  <|> P.Capper <$ "capper"

joiner_ = "joiner"
  <=> (\c -> P.Joiner [c]) <$ "join" <*> char
  <|> P.Joiner "_"  <$ "snake"
  <|> P.Joiner "-"  <$ "dash"
  <|> P.Joiner "/"  <$ "file"
  <|> P.Joiner ""   <$ "squeeze"
  <|> P.CamelJoiner <$ "camel"
  <|> P.ClassJoiner <$ "class"

brackets_ = "brackets"
  <=> P.bracket          <$ "round" <*> char
  <|> P.Brackets "(" ")" <$ "par"
  <|> P.Brackets "[" "]" <$ "square"
  <|> P.Brackets "{" "}" <$ "curl"
  <|> P.Brackets "<" ">" <$ "angle"
  <|> P.bracket '"'      <$ "string"
  <|> P.bracket '\''     <$ "ticked"
  <|> P.bracket '|'      <$ "norm"


inlineRHS = set (_RHSInfo.dnsInline) True

dictation_ = inlineRHS $ "dictation_"
  <=> (P.Dictation . fmap T.unpack) <$> liftLeaf (some anyWord) (SomeDNSNonTerminal$ DNSBuiltinRule DGNDictation)
  -- <=> P.Dictation <$> liftLeaf (some anyWord) (SomeDNSNonTerminal$ DNSBuiltinRule DGNDictation)
{-# NOINLINE dictation_ #-} --TODO doesn't help with the unshared <dictation__4>/<dictation__14>/<dictation__16>

word_ = inlineRHS $ "word_"
 <=> T.unpack <$> liftLeaf anyWord (SomeDNSNonTerminal$ DNSBuiltinRule DGNWords)
 -- <=> liftLeaf anyWord (SomeDNSNonTerminal$ DNSBuiltinRule DGNWords)

keyword_ = inlineRHS $ "keyword_"
 <=> T.unpack <$> liftLeaf anyWord (SomeDNSNonTerminal$ DNSBuiltinRule DGNWords)
 -- <=> id <$> liftLeaf anyWord (SomeDNSNonTerminal$ DNSBuiltinRule DGNWords)
 -- <=> Keyword <$> word_


-- ================================================================ --

renameDNSEarleyFunc
 :: forall z m n1 n2 t f1 f2 a. ((f1 ~ DNSEarleyFunc z n1 t), (f2 ~ DNSEarleyFunc z n2 t))
 => (Applicative m)
 => (forall x. RHS n1 t f1 x -> n1 t f1 x -> m (    n2 t f2 x))
 -> (                       RHS n1 t f1 a -> m (RHS n2 t f2 a))
renameDNSEarleyFunc u = \case
 k@(NonTerminal x r)  ->  NonTerminal <$> u k x <*> go r -- like traverse, except this case
 Terminal i r       ->  pure$ Terminal i r
 Opt  i r           ->  Opt  i <$> go r
 Many i r           ->  Many i <$> go r
 Some i r           ->  Some i <$> go r
 Pure a             ->  pure$ Pure a
 r `Apply` x        ->  Apply <$> go r <*> (case x of
  TreeRHS pRHS rRHS ->  TreeRHS <$> go pRHS <*> go rRHS
  LeafRHS p s       ->  pure$ LeafRHS p s)
 r :<*> r'          ->  (:<*>) <$> go r <*> go r'
 Alter rs           ->  Alter <$> go `traverse` rs
 where
 go :: forall x. RHS n1 t f1 x -> m (RHS n2 t f2 x)
 go = renameDNSEarleyFunc u

renameDNSEarleyRHSST
 :: forall z s n1 n2 t f1 f2 a. ((f1 ~ DNSEarleyFunc z n1 t), (f2 ~ DNSEarleyFunc z n2 t))
 => (forall x. RHS n1 t f1 x -> n1 t f1 x -> ST s (    n2 t f2 x))
 -> ST s                   (RHS n1 t f1 a -> ST s (RHS n2 t f2 a))
renameDNSEarleyRHSST u = unsafeIOToST$ do
 c <- HRefCache.newCache
 -- return$ renameRHS'$ \r1 n r2 -> unsafeIOToST$ do
 return$ renameDNSEarleyFunc$ \r1 n -> unsafeIOToST$ do
  k <- HRefCache.forceStableName r1
  readIORef c >>= (HRefCache.lookupRef k >>> traverse readIORef) >>= \case
   Just y  -> return y          -- cache hit
   Nothing -> do                -- cache miss
    y <- unsafeSTToIO$ u r1 n
    v <- newIORef y
    _ <- atomicModifyIORef' c ((,()) . HRefCache.insertRef k v)
    return y

renameDNSEarleyRHSIO
 :: ((f1 ~ DNSEarleyFunc z n1 t), (f2 ~ DNSEarleyFunc z n2 t))
 => (forall x. RHS n1 t f1 x -> n1 t f1 x -> IO (    n2 t f2 x))
 -> IO                   (RHS n1 t f1 a -> IO (RHS n2 t f2 a))
renameDNSEarleyRHSIO u = do
 c <- HRefCache.newCache
 return$ renameDNSEarleyFunc$ \r1 n -> do
  k <- HRefCache.forceStableName r1
  readIORef c >>= (HRefCache.lookupRef k >>> traverse readIORef) >>= \case
   Just y  -> return y          -- cache hit
   Nothing -> do                -- cache miss
    y <- u r1 n
    v <- newIORef y
    _ <- atomicModifyIORef' c ((,()) . HRefCache.insertRef k v)
    return y

renameRHSST
 :: (forall x. RHS n t f x -> n t f x -> ST s (n' t f x))
 -- :: (forall x. RHS n t f x -> n t f x -> RHS n t f x -> ST s (n' t f x))
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
 -- return$ renameRHS$ \r1 n r2 -> unsafeIOToST$ do
 return$ renameRHS'$ \r1 n -> unsafeIOToST$ do
  k <- HRefCache.forceStableName r1
  readIORef c >>= (HRefCache.lookupRef k >>> traverse readIORef) >>= \case
   Just y  -> return y          -- cache hit
   Nothing -> do                -- cache miss
    y <- unsafeSTToIO$ u r1 n
    v <- newIORef y
    _ <- atomicModifyIORef' c ((,()) . HRefCache.insertRef k v)
    return y
-- renaming recursive RHS doesn't terminate because: in renaming/traversing the non-terminal, the name is decoupled from the body. the natural transformation should return a pair, and the name and the rhs are both cached. makes sense, as we are caching on the non-terminal (isomorphic to a pair) pointer.


-- ================================================================ --

data EarleyName z n t (f :: * -> *) a = EarleyName
 { unEarleyName :: E.Prod z n t a -> E.Prod z n t a
 }
-- not a Functor

-- renameRHSToEarley
--  :: forall s r n t f a. ()
--  => ST s (        RHS (ConstName                 n) t f a
--          -> ST s (RHS (EarleyName (E.Rule s r) n) t f a)
--          )
-- renameRHSToEarley = renameRHSST $ \_ (ConstName n) _ -> do
renameRHSToEarley :: ST s (RHS (ConstName (_, n)) t (DNSEarleyFunc z (ConstName (_, n)) t) a -> ST s (RHS (EarleyName (E.Rule s r) n) t (DNSEarleyFunc z (EarleyName (E.Rule s r) n) t) a))
renameRHSToEarley = renameDNSEarleyRHSST $ \_ (ConstName (_, n)) -> do
 conts <- newSTRef =<< newSTRef []
 null  <- newSTRef Nothing
 return$ EarleyName (\p -> E.NonTerminal (E.Rule p null conts) (E.Pure id) E.<?> n)

induceEarley
 :: forall s r n t a z. ((z ~ E.Rule s r), (n ~ String))  -- type equality only for documentation
 => (Eq t)
 => RHS (EarleyName z n)
        t
        (DNSEarleyFunc z (EarleyName z n) t)
        a
 -> E.Prod z n t a
induceEarley = runRHS
 (\n r -> (unEarleyName n) (induceEarley r))  -- accessor (not pattern match) for polymorphic z (Rank2 elsewhere)
 E.symbol
 (\case
  LeafRHS p    _ -> p
  TreeRHS pRHS _ -> induceEarley pRHS)

buildEarley
 :: E.Prod (E.Rule s a) n t a
 -> [t]
 -> ST s (E.Result s n [t] a)
buildEarley p xs = do
  s <- E.initialState p
  E.parse [s] [] [] (return ()) [] 0 xs

runEarley
 :: (Eq t)
 => (forall r. RHS (ConstName (_,String)) t (DNSEarleyFunc (E.Rule s r) (ConstName (_,String)) t) a)
 -> [t]
 -> ST s (E.Result s String [t] a)
runEarley r1 xs = do
 r2 <- renameRHSToEarley >>= ($ r1)
 buildEarley (induceEarley r2) xs

parse
 :: (Eq t)
 => (forall r. RHS (ConstName (_,String)) t (DNSEarleyFunc r (ConstName (_,String)) t) a)
 -> [t]
 -> ([a], E.Report String [t])
parse r xs = E.fullParses$ runEarley r xs

parseString
 :: (forall r. DNSEarleyRHS r a)
 -> Text
 -> [a]
parseString r s = as
 where (as,_) = E.fullParses$ runEarley r (T.words s)

parsePhrase :: Text -> String
parsePhrase
 = (flip P.mungePhrase) P.defSpacing
 . (flip P.splatPasted) "clipboard contents"
 . P.pPhrase
 . argmax qPhrase
 . parseString phrase_

-- argmax :: (a -> Int) -> (a -> a -> Ordering)
-- argmax f = maximumBy (comparing `on` f)
-- argmax f = maximumBy (\x y -> f x `compare` f y)
argmax f = maximumBy (compare `on` f)

-- the specificity ("probability") of the phrase parts. bigger is better.
qPhrase :: [P.Phrase_] -> Int
qPhrase = sum . fmap (\case
 Escaped_ _ -> 1
 Quoted_ _ -> 1
 Pasted_  -> 1
 Blank_  -> 1
 Spelled_ _ -> 1
 Capped_ _ -> 1
 Separated_ _ -> 1
 Cased_ _ -> 1
 Joined_ _ -> 1
 Surrounded_ _ -> 1
 Dictated_ _ -> 0)



-- ================================================================ --

-- | a directly-recursive right-hand side, with a left-hand side annotation; like a production.
type DNSRHSRec i t n = Cofree (DNSRHS t) (i, n)
-- DNSRHS t (Cofree (DNSRHS t) (i, n))

data DNSUniqueName n t (f :: * -> *) a = DNSUniqueName DNSInfo n Int

renameRHSToDNS :: IO (RHS (DNSEarleyName n) t (DNSEarleyFunc z (DNSEarleyName n) t) a -> IO (RHS (DNSUniqueName n) t (DNSEarleyFunc z (DNSUniqueName n) t) a))
renameRHSToDNS = renameDNSEarleyRHSIO $ \_ (ConstName (i, n)) -> do
 k <- hashUnique <$> newUnique
 return$ DNSUniqueName i n k

induceDNS
 :: RHS (DNSUniqueName String) t (DNSEarleyFunc z (DNSUniqueName String) t) a
 -> Cofree (DNSRHS t) (DNSInfo, String)
induceDNS = induceDNS' >>> \case
 SomeDNSNonTerminal (DNSRule ((i,n) :< r)) -> (i,n)         :< r
 r                                         -> defaultDNSLHS :< r

defaultDNSLHS :: (DNSInfo,String)
defaultDNSLHS = (defaultDNSInfo,"defaultDNSLHS") -- TODO should be unique; quote it?

induceDNS' -- TODO doesn't terminate on cyclic data
 -- :: RHS (DNSUniqueName n) t (DNSEarleyFunc z (DNSUniqueName n) t) a
 :: RHS (DNSUniqueName String) t (DNSEarleyFunc z (DNSUniqueName String) t) a
 -- -> (Cofree (DNSRHS t) (Maybe (i,n)))
 -> DNSRHS t (Cofree (DNSRHS t) (DNSInfo, String))
induceDNS' = foldRHS
  (\(DNSUniqueName i n k) r -> SomeDNSNonTerminal$ DNSRule$ (i, n <> "_" <> (show k)) :< r)
  (DNSTerminal . DNSToken)
  (\case
   LeafRHS _ g -> unVoidDNSRHS g
   TreeRHS _ gRHS -> induceDNS' gRHS) -- auxiliary recursion, not a cata
  (UnitDNSRHS)
  (\r1 r2 -> DNSSequence (r1 :| [r2]))
  (maybe ZeroDNSRHS DNSAlternatives . NonEmpty.nonEmpty)
  (DNSOptional)
  (DNSOptional . DNSMultiple)
  (DNSMultiple)

unVoidDNSRHS :: DNSRHS t Void -> DNSRHS t n
unVoidDNSRHS = second (\case)

-- 1. collect Cofree's by name
-- 2. for each, project Cofree's to name
reifyDNSRHS :: forall i n t. (Eq n) => Cofree (DNSRHS t) (i,n) -> NonEmpty (DNSProduction i t n)
--  Map n (DNSProduction i t n) is more efficient but unordered
reifyDNSRHS = NonEmpty.fromList            --   TODO prove safety
 . fmap (snd >>> toIndirectDNSRHS >>> toDNSProduction)
 . (flip execState) []
 . go -- runMaybeT
 where

 toIndirectDNSRHS :: Cofree (DNSRHS t) (i,n) -> (i, n, DNSRHS t n)
 toIndirectDNSRHS ((i,n) :< r) = (i,n, bimap id (\((_,n) :< _) -> n) r)

 toDNSProduction :: (i, n, DNSRHS t n) -> DNSProduction i t n
 toDNSProduction (i,n,r) = DNSProduction i (DNSRule n) r

 list'insertBy :: (a -> a -> Bool) -> a -> [a] -> [a]
 list'insertBy eq x xs = case List.find (eq x) xs of
  Nothing -> snoc xs x          -- could reverse later; or like fuse it with a fold; or difference list; performance here doesn't matter
  Just{}  ->    xs

 go :: Cofree (DNSRHS t) (i,n) -> (State [(n, Cofree (DNSRHS t) (i,n))]) ()
 go c@((_,n) :< r) = do
  gets (List.lookup n) >>= \case
   Nothing -> do
    _ <- modify$ list'insertBy ((==) `on` fst) (n, c)
    _ <- bitraverse return go r
    return()
   Just {} -> return()

serializeDNSGrammar' :: DNSGrammar DNSInfo Text Text -> Either [SomeException] SerializedGrammar
serializeDNSGrammar' uG = do
 let oG = uG                    -- optimizeDNSInfoGrammar
 eG <- escapeDNSGrammar oG
 let sG = serializeGrammar eG
 return$ sG

formatRHS :: RHS (DNSEarleyName String) Text (DNSEarleyFunc z (DNSEarleyName String) Text) a -> IO (Either [SomeException] SerializedGrammar)
formatRHS r = do
 renamer <- renameRHSToDNS
 let serializeRHS = (induceDNS >>> reifyDNSRHS >>> defaultDNSGrammar >>> second T.pack >>> serializeDNSGrammar')
 renamer r >>= (serializeRHS >>> return)

showRHS :: RHS (DNSEarleyName String) Text (DNSEarleyFunc z (DNSEarleyName String) Text) a -> IO Text
showRHS r = do
 eG <- formatRHS r
 return$ either (T.pack . show) displaySerializedGrammar eG


-- ================================================================ --

-- -- TODO compare relative associativity
-- infixl 4 <#> -- TODO   `(#) = review` or something in lens
-- infixl 4 # -- TODO   `(&) = flip ($)` in base in 7.10


-- -- | like '<$' or '<$>', given the type (see the 'AppRHS' instances in this module).
-- --
-- -- e.g. inference for @True <#> "true"@ (__without__ @OverloadedStrings@):
-- --
-- -- @
-- -- (<#>) :: (AppRHS p r l i a) => LeftR a b -> a -> RHS b
-- -- -- given string literal ("true" :: String)
-- -- a ~ String
-- -- (<#>) :: (AppRHS String) => LeftR String b -> String -> RHS b
-- -- -- accept constraint 'AppRHS' and expand type family 'LeftR'
-- -- (<#>) :: b -> String -> RHS b
-- -- @
-- --
-- (<#>) :: (Functor (p i), AppRHS p r l i a) => LeftR a b -> a -> RHS p r l i b
-- f <#> x = pure f `appR` x

-- -- | like '<*' or '<*>', given the type (see the 'AppRHS' instances in this
-- -- module).
-- --
-- -- e.g. inference for @TODO@ (__without__ @OverloadedStrings@):
-- --
-- -- @
-- (#) :: (Functor (p i), AppRHS p r l i a) => RHS p r l i (LeftR a b) -> a -> RHS p r l i b
-- (#) = appR

-- -- | specialized 'appR' has types:
-- --
-- -- * @a        -> String    -> RHS a@
-- -- * @(a -> b) -> Rule p r a -> RHS b@
-- -- * @(a -> b) -> RHS a     -> RHS b@
-- -- * etc.
-- --
-- class (ToRHS p r l i a) => AppRHS p r l i a where
--  type LeftR a b :: *
--  appR :: RHS p r l i (LeftR a b) -> a -> RHS p r l i b

-- instance (Functor (p String)) => AppRHS p r l String String            where
--  type LeftR String b              = b
--  appR f x = f <*  toR x
--  -- the equality constraint delays unification until after the instance head is committed to,
--  -- e.g. (p2 ~ EarleyProduction z) needs this; the z's universally quantified and unconstrained, and don't unify with each other when an RHS is defined.
-- instance ((p1 ~ p2), Functor (p2 i)) => AppRHS p1 r l i (RHS p2 r l i a) where
--  type LeftR (RHS p r l i a) b         = (a -> b)
--  appR f x = f <*>     x

-- -- | inject @a@s of different types into an @RHS@.
-- --
-- -- the first parameters (i.e. @p@ and @r@) are always abstract;
-- -- they seem to be needed to be in scope to unify with themselves in @a@.
-- class ToRHS p r l i a where
--  type ToR a :: *
--  toR :: a -> RHS p r l i (ToR a)

-- instance              ToRHS p  r l String String              where
--   type ToR String                   = String;
--   toR = word
-- instance (p1 ~ p2) => ToRHS p1 r l i (RHS     p2 r   l i   a) where
--   type ToR (RHS     p2 r   l i   a) = a;
--   toR = id
-- -- instance (IsString i) => ToRHS p r l i String            where  type ToR String            = String;  toR = word . fromString


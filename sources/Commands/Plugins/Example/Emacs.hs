{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable, DeriveFunctor     #-}
{-# LANGUAGE ExistentialQuantification, ExtendedDefaultRules           #-}
{-# LANGUAGE FlexibleContexts, KindSignatures, LambdaCase              #-}
{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, PatternSynonyms #-}
{-# LANGUAGE PostfixOperators, RankNTypes, ScopedTypeVariables         #-}
{-# LANGUAGE StandaloneDeriving, TupleSections, TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances                                      #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-imports -fno-warn-type-defaults -fno-warn-partial-type-signatures #-}
module Commands.Plugins.Example.Emacs where
import           Commands.Etc
import           Commands.RHS.Types
import qualified Data.HRefCache.Internal         as HRefCache
-- import Commands.Plugins.Example.Phrase hiding  (phrase, casing, separator, joiner, brackets, keyword, char, dictation, word)
import           Commands.Plugins.Example.Phrase (Phrase_ (..))
import qualified Commands.Plugins.Example.Phrase as P
import Commands.Frontends.Dragon13.Types

import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NonEmpty
import qualified Text.Earley                     as E
import qualified Text.Earley.Grammar             as E
import qualified Text.Earley.Internal            as E
import BasePrelude hiding (bracket, Product (..))

import Prelude ()
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
import Data.Functor.Foldable (Fix(..))


type DNSProductionFix i t = Fix (DNSProduction i t)

type RHSEarleyDNS z = RHS
 (ConstName (DNSInfo, String))
 String
 (RHSEarleyDNSF z (ConstName (DNSInfo, String)) String)

data RHSEarleyDNSF z n t a
 =           LeafRHS (E.Prod z String t a) (DNSRHS t String)
 | forall x. TreeRHS (RHS n t (RHSEarleyDNSF z n t) a) (RHS n t (RHSEarleyDNSF z n t) x)
-- couples parser (E.Prod) with format (DNSRHS) with (ConstName) :-(
deriving instance (Functor (n t (RHSEarleyDNSF z n t))) => Functor (RHSEarleyDNSF z n t) -- TODO UndecidableInstances
-- Variables ‘n, t’ occur more often than in the instance head in the constraint

data DNSFixName t = DNSFixName (DNSProduction DNSInfo (DNSFixName t) t)

(<=>) :: String -> RHSEarleyDNS z a -> RHSEarleyDNS z a
-- type signature for type inference, disambiguates:
--  "No instance for (Data.String.IsString _)" and "No instance for (Functor _)"
(<=>) n r = NonTerminal (ConstName (defaultDNSInfo, n)) r
infix 2 <=>

liftLeaf p r = liftRHS (LeafRHS p r)
liftTree p r = liftRHS (TreeRHS p r)

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

dictation_ = "dictation_"
  <=> P.Dictation <$> liftLeaf (some anyWord) (SomeDNSNonTerminal$ DNSBuiltinRule DGNDictation)

word_ = "word_"
 <=> liftLeaf anyWord (SomeDNSNonTerminal$ DNSBuiltinRule DGNWords)

keyword_ = "keyword_"
 <=> id <$> liftLeaf anyWord (SomeDNSNonTerminal$ DNSBuiltinRule DGNWords)
 -- <=> Keyword <$> word_

renameRHSEarleyDNSF
 :: forall z m n1 n2 t f1 f2 a. ((f1 ~ RHSEarleyDNSF z n1 t), (f2 ~ RHSEarleyDNSF z n2 t))
 => (Applicative m)
 => (forall x. RHS n1 t f1 x -> n1 t f1 x -> m (    n2 t f2 x))
 -> (                       RHS n1 t f1 a -> m (RHS n2 t f2 a))
renameRHSEarleyDNSF u = \case
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
 go = renameRHSEarleyDNSF u

renameRHSEarleyDNSST
 :: forall z s n1 n2 t f1 f2 a. ((f1 ~ RHSEarleyDNSF z n1 t), (f2 ~ RHSEarleyDNSF z n2 t))
 => (forall x. RHS n1 t f1 x -> n1 t f1 x -> ST s (    n2 t f2 x))
 -> ST s                   (RHS n1 t f1 a -> ST s (RHS n2 t f2 a))
renameRHSEarleyDNSST u = unsafeIOToST$ do
 c <- HRefCache.newCache
 -- return$ renameRHS'$ \r1 n r2 -> unsafeIOToST$ do
 return$ renameRHSEarleyDNSF$ \r1 n -> unsafeIOToST$ do
  k <- HRefCache.forceStableName r1
  readIORef c >>= (HRefCache.lookupRef k >>> traverse readIORef) >>= \case
   Just y  -> return y          -- cache hit
   Nothing -> do                -- cache miss
    y <- unsafeSTToIO$ u r1 n
    v <- newIORef y
    _ <- atomicModifyIORef' c ((,()) . HRefCache.insertRef k v)
    return y

renameRHSEarleyDNSIO
 :: ((f1 ~ RHSEarleyDNSF z n1 t), (f2 ~ RHSEarleyDNSF z n2 t))
 => (forall x. RHS n1 t f1 x -> n1 t f1 x -> IO (    n2 t f2 x))
 -> IO                   (RHS n1 t f1 a -> IO (RHS n2 t f2 a))
renameRHSEarleyDNSIO u = do
 c <- HRefCache.newCache
 return$ renameRHSEarleyDNSF$ \r1 n -> do
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



data EarleyName z n t (f :: * -> *) a = EarleyName
 { unEarleyName :: E.Prod z n t a -> E.Prod z n t a
 }
-- not a Functor

-- deriveEarley
--  :: forall s r n t f a. ()
--  => ST s (        RHS (ConstName                 n) t f a
--          -> ST s (RHS (EarleyName (E.Rule s r) n) t f a)
--          )
-- deriveEarley = renameRHSST $ \_ (ConstName n) _ -> do
deriveEarley = renameRHSEarleyDNSST $ \_ (ConstName (_, n)) -> do
 conts <- newSTRef =<< newSTRef []
 null  <- newSTRef Nothing
 return$ EarleyName (\p -> E.NonTerminal (E.Rule p null conts) (E.Pure id) E.<?> n)

induceEarley
 :: forall s r n t a z. ((z ~ E.Rule s r), (n ~ String))  -- type equality only for documentation
 => (Eq t)
 => RHS (EarleyName z n)
        t
        (RHSEarleyDNSF z (EarleyName z n) t)
        a
 -> E.Prod z n t a
induceEarley = runRHS
 (\n r -> (unEarleyName n) (induceEarley r))
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
 => (forall r. RHS (ConstName (_,String)) t (RHSEarleyDNSF (E.Rule s r) (ConstName (_,String)) t) a)
 -> [t]
 -> ST s (E.Result s String [t] a)
runEarley r1 xs = do
 r2 <- deriveEarley >>= ($ r1)
 buildEarley (induceEarley r2) xs

parse
 :: (Eq t)
 => (forall r. RHS (ConstName (_,String)) t (RHSEarleyDNSF r (ConstName (_,String)) t) a)
 -> [t]
 -> ([a], E.Report String [t])
parse r xs = E.fullParses$ runEarley r xs

parseString
 :: (forall r. RHSEarleyDNS r a)
 -> String
 -> [a]
parseString r s = as
 where (as,_) = E.fullParses$ runEarley r (words s)

parsePhrase :: String -> String
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



data DNSName n t (f :: * -> *) a = DNSName DNSInfo n Int

instance (Eq n) => (Eq  (DNSName n t f a)) where (==) = eqDNSName
instance (Eq n) => (Eq1 (DNSName n t f))   where eq1  = eqDNSName
instance (Eq n) => (HEq (DNSName n t f))   where hEq  = eqDNSName

class HEq f where
 hEq :: f a -> f b -> Bool
 -- default hEq :: f a -> f b -> Bool

eqDNSName :: (Eq n) => DNSName n t f a -> DNSName n t' f' a' -> Bool
eqDNSName (DNSName i n k) (DNSName i' n' k')
  = i == i'
 && n == n'
 && k == k'

-- deriveDNS ::
deriveDNS = renameRHSEarleyDNSIO $ \_ (ConstName (i, n)) -> do
 k <- hashUnique <$> newUnique
 return$ DNSName i n k

-- also collect names to build DNSProductions
-- induceDNS
--  :: RHS (DNSName n) t (RHSEarleyDNSF z (DNSName n) t) a
--  -> DNSRHS t (DNSProduction DNSInfo n t)
-- induceDNS = foldRHS
--  (\(DNSName i n k) r -> SomeDNSNonTerminal$ DNSFixName$ DNSProduction i (DNSRule $ n <> show k) r)
--  DNSTerminal
--  (\case
--   LeafRHS _ g  -> g
--   TreeRHS _ gRHS -> induceDNS gRHS)
--  UnitDNSRHS
--  (\r1 r2 -> DNSSequence (r1 :| [r2]))
--  (maybe ZeroDNSRHS DNSAlternatives . NonEmpty.nonEmpty)
--  (DNSOptional)
--  (DNSOptional . DNSMultiple)
--  (DNSMultiple)

-- reifyDNS :: DNSRHS t (DNSProduction i n t) -> Map n (DNSRHS n t)
-- reifyDNS r = undefined r

-- reifyRHS :: RHS n t f x -> Map (Exists (n t f)) (SomeRHS n t f)
-- reifyRHS :: (?eqName :: forall a b. (n t f a) -> (n t f b) -> Bool) => 
-- reifyRHS :: (?hEqI :: HEqI (n t f)) =>
-- (Eq n)  -- (HEq (DNSName n t f))  -- (Eq1 (n t f))  -- (forall x. Eq (n t f x))
-- reifyRHS :: (Eq1 (n t f)) => RHS n t f x -> [Exists (Product (n t f) (RHS n t f))]
reifyRHS :: (forall a b. (n t f a) -> (n t f b) -> Bool) -> RHS n t f x -> OMap (n t f) (RHS n t f)
reifyRHS eqName r = undefined execState eqName r

type HEqI f = forall x y. f x -> f y -> Bool
leftProduct (Pair f _) = f
rightProduct (Pair _ g) = g
-- | list with higher-order items
type OMap f g = [Exists (Product f g)]
-- insertOMap :: f a -> g a -> OMap f g -> OMap f g
-- insertOMap f g fgs = Exists (Pair f g) : fgs
insertOMap :: (forall x. (f x, g x)) -> OMap f g -> OMap f g
insertOMap fg fgs = Exists (Pair (fst fg) (snd fg)) : fgs
-- insertOMap (f,g) fgs = Exists (Pair f g) : fgs  -- type variable ‘x’ would escape its scope
lookupOMap :: (HEqI f) -> OMap f g -> f a -> Maybe (g a)
-- lookupOMap hEq fgs f1 =  case (\(Exists (Pair f2 _)) -> (hEq f1 f2)) fgs of (Exists (Pair _ g)) -> g
lookupOMap hEq fgs f = foldr go Nothing fgs
 where
 go fg = \case
  Nothing -> if (hEq f . leftProduct . unExists) fg then Just$ (rightProduct . unExists) fg else Nothing
  Just r -> Just r



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
 -- _phrase <- unsafeSTToIO (deriveEarley >>= ($ phrase))
 putStrLn ""

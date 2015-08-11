{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable, DeriveFunctor, DeriveAnyClass     #-}
{-# LANGUAGE ExistentialQuantification              #-}
{-# LANGUAGE FlexibleContexts, KindSignatures, LambdaCase              #-}
{-# LANGUAGE PartialTypeSignatures, PatternSynonyms #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, EmptyCase         #-}
{-# LANGUAGE StandaloneDeriving, TupleSections, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ConstraintKinds  #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Commands.Mixins.DNS13OSX9
 ( module Commands.Mixins.DNS13OSX9
 , module Commands.RHS.Types
 ) where

import           Commands.Etc
import           Commands.Munging
import           Commands.RHS.Types
import qualified Data.HRefCache.Internal         as HRefCache
-- import Commands.Plugins.Example.Phrase hiding  (phrase, casing, separator, joiner, brackets, keyword, char, dictation, word)
import Commands.Frontends.Dragon13
import qualified Commands.Backends.OSX as OSX

import Data.Void
import Data.Bifunctor
import Data.Bitraversable
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NonEmpty
import qualified Text.Earley                     as E
import qualified Text.Earley.Grammar             as E
import qualified Text.Earley.Internal            as E
import qualified Data.Text.Lazy as T
import Control.Lens hiding (snoc, (#))
import Control.Monad.Catch (MonadThrow (..))

import           Data.Char
import Control.Exception (Exception (..), SomeException (..))
import Data.Monoid              ((<>))
import           Control.Arrow                   ((>>>))
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.Foldable
import           Data.Function                   (on)
import           Data.IORef
import           Data.Proxy
import           Data.Typeable
import           Data.STRef
import           Data.Unique
import Control.Monad.Trans.State
import Control.Comonad.Cofree
import qualified Data.List as List
import GHC.Exts (IsString(..))
import           Language.Haskell.TH.Syntax      (Name)

--  ( module Commands.Mixins.DNS13OSX9.Types
--  -- , module Commands.Mixins.OSX9.Types
--  , module Commands.Mixins.DNS13OSX9.Primitive
--  , module Commands.Mixins.DNS13OSX9.Combinator
--  , module Commands.Sugar
--  ) where
-- import Commands.Mixins.DNS13OSX9.Types
-- -- import Commands.Mixins.OSX9.Types
-- import Commands.Mixins.DNS13OSX9.Combinator
-- import Commands.Mixins.DNS13OSX9.Primitive
-- import Commands.Sugar


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
deriving instance (Functor (n t (DNSEarleyFunc z n t))) => Functor (DNSEarleyFunc z n t) --TODO UndecidableInstances
-- Variables ‘n, t’ occur more often than in the instance head in the constraint

data DNSFixName t = DNSFixName (DNSProduction DNSInfo (DNSFixName t) t) --TODO newtype

-- | @ConstraintKinds@
type Functor'RHS n t f = (Functor (n t f), Functor f)

liftLeaf :: forall a
                         (n :: * -> (* -> *) -> * -> *)
                         t
                         (z :: * -> * -> * -> *)
                         (n1 :: * -> (* -> *) -> * -> *)
                         t1.
                  E.Prod z String t1 a
                  -> DNSRHS t1 Void -> RHS n t (DNSEarleyFunc z n1 t1) a
liftLeaf p r = liftRHS (LeafRHS p r)

liftTree :: forall a
                         (n :: * -> (* -> *) -> * -> *)
                         t
                         (z :: * -> * -> * -> *)
                         (n1 :: * -> (* -> *) -> * -> *)
                         t1
                         x.
                  RHS n1 t1 (DNSEarleyFunc z n1 t1) a
                  -> RHS n1 t1 (DNSEarleyFunc z n1 t1) x
                  -> RHS n t (DNSEarleyFunc z n1 t1) a
liftTree p r = liftRHS (TreeRHS p r)

anyWord :: E.Prod z String Text Text --TODO  t t
anyWord = E.Terminal (const True) (pure id)

anyLetter :: E.Prod z String Text Text
anyLetter = (E.satisfy (T.all isUpper)) E.<?> "letter"

_RHSInfo :: Traversal' (RHS (DNSEarleyName String) t f a) DNSInfo
_RHSInfo = _NonTerminal._1.unConstName._1


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

type EarleyParser_ a = forall r. RULED EarleyParser r a

data EarleyParser z a = EarleyParser
 { pProd :: E.Prod z String Text a
 , pBest :: NonEmpty a -> a 
 }

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
-- renameRHSToEarley :: ST s (RHS (ConstName (_, n)) t (DNSEarleyFunc z (ConstName (_, n)) t) a -> ST s (RHS (EarleyName (E.Rule s r) n) t (DNSEarleyFunc z (EarleyName (E.Rule s r) n) t) a))
renameRHSToEarley
 :: ST s (        DNSEarleyRHS (E.Rule s r) a
         -> ST s (RHS (EarleyName (E.Rule s r) String)
                      Text
                      (DNSEarleyFunc (E.Rule s r)
                      (EarleyName (E.Rule s r) String) Text)
                      a)
         )
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
buildEarley p ts = do
  s <- E.initialState p
  E.parse [s] [] [] (return ()) [] 0 ts

runEarley
 :: (forall r. RHS (ConstName (_,String)) Text (DNSEarleyFunc (E.Rule s r) (ConstName (_,String)) Text) a)
 -> [Text]
 -> ST s (E.Result s String [Text] a)
-- runEarley
--  :: (Eq t)
--  => (forall r. RHS (ConstName (_,String)) t (DNSEarleyFunc (E.Rule s r) (ConstName (_,String)) t) a)
--  -> [t]
--  -> ST s (E.Result s String [t] a)
runEarley r1 ts = do
 r2 <- renameRHSToEarley >>= ($ r1)
 buildEarley (induceEarley r2) ts

parseRaw
 :: (forall r. RHS (ConstName (_,String)) Text (DNSEarleyFunc r (ConstName (_,String)) Text) a)
 -> [Text]
 -> ([a], E.Report String [Text])
parseRaw r ts = E.fullParses$ runEarley r ts

type EarleyEither e t = Either (E.Report e [t])

e'ParseBest :: EarleyParser_ a -> [Text] -> EarleyEither String Text a
e'ParseBest p ts = do
 parses <- toEarleyEither (E.fullParses (buildEarley (p&pProd) ts))
 return$ (p&pBest) parses
 where
 -- report = E.fullParses result
 -- result = buildEarley (p&pProd) ts

-- | may 'throwM' a @('E.Report' String Text)@
parseThrow
 :: (forall z. DNSEarleyRHS z a)
 -> [Text]
 -> Possibly (NonEmpty a)
parseThrow r = parseEither r >>> \case
 Left  e  -> throwM e
 Right xs -> return xs

parseEither
 :: (forall r. RHS (ConstName (_,String)) Text (DNSEarleyFunc r (ConstName (_,String)) Text) a)
 -> [Text]
 -> EarleyEither String Text (NonEmpty a)
parseEither r
 = toEarleyEither
 . parseRaw r

-- | refine an 'E.Report', forcing the results.
-- 'Right' when there is at least one parse that has consumed the whole input.
toEarleyEither
 :: ([a], E.Report e [t])
 -> EarleyEither e t (NonEmpty a)
toEarleyEither = \case
 ([],   e)               -> Left  e
 (x:xs, E.Report _ _ []) -> Right (x:|xs)
 (_,    e)               -> Left  e

parseText
 :: (forall r. DNSEarleyRHS r a) -- Couldn't match type ‘z1’ with ‘r’ because type variable ‘r’ would escape its scope
 -> [Text]
 -> EarleyEither String Text (NonEmpty a)
parseText r = parseEither r --  TODO

parseBest
 :: (NonEmpty a -> a)
 -> (forall r. DNSEarleyRHS r a)
 -> [Text]
 -> EarleyEither String Text a --TODO use Possibly?
parseBest best r = second best . parseText r

parseList :: (forall r. DNSEarleyRHS r a) -> [Text] -> [a]
parseList r ts = as
 where (as,_) = parseRaw r ts

parseString :: (forall r. DNSEarleyRHS r a) -> String -> [a]
parseString r = parseList r . (T.words . T.pack)

argmax :: Ord b => (a -> b) -> (NonEmpty a -> a)
argmax f = maximumBy (compare `on` f)
-- argmax :: (a -> Int) -> (a -> a -> Ordering)
-- argmax f = maximumBy (comparing `on` f)
-- argmax f = maximumBy (\x y -> f x `compare` f y)




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
 let oG = optimizeDNSInfoGrammar uG                    -- optimizeDNSInfoGrammar
 eG <- escapeDNSGrammar oG
 let sG = serializeGrammar eG
 return$ sG

formatRHS :: RHS (DNSEarleyName String) Text (DNSEarleyFunc z (DNSEarleyName String) Text) a -> IO (Either [SomeException] SerializedGrammar)
formatRHS r = do
 renamer <- renameRHSToDNS
 let serializeRHS = (induceDNS >>> reifyDNSRHS >>> defaultDNSGrammar >>> second T.pack >>> serializeDNSGrammar')
 eG <- renamer r >>= (serializeRHS >>> return)
 return$ eG

showRHS :: RHS (DNSEarleyName String) Text (DNSEarleyFunc z (DNSEarleyName String) Text) a -> IO Text
showRHS r = do
 eG <- formatRHS r
 return$ either (T.pack . show) displaySerializedGrammar eG


-- ================================================================ --

-- TODO compare relative associativity
infixl 4 <#>
infixl 4 #


{- | like '<$' or '<$>'.

@f <#> x = pure f `'appRHS'` x@

specialized:

@
(<#>) :: a        -> String      -> RHS n t f a@
(<#>) :: (x -> a) -> RHS n t f x -> RHS n t f a@
@

inferable/unambiguous, but only without @OverloadedStrings@.

-}
(<#>) :: ((Functor (n t f)), Functor f, AppRHS n t f x) => LeftRHS x a -> x -> RHS n t f a
f <#> x = pure f `appRHS` x

{- | like '<*' or '<*>'.

inferable/unambiguous, but only without @OverloadedStrings@.

-}
(#) :: (Functor f, AppRHS n t f x) => RHS n t f (LeftRHS x a) -> x -> RHS n t f a
(#) = appRHS

{- | sugar to ignore the result of strings (injected into right-hand sides), 
but apply the result of right-hand sides themselves.

specialized:

* @appRHS :: RHS n t f a        -> String      -> RHS n t f a@
* @appRHS :: RHS n t f (x -> a) -> RHS n t f x -> RHS n t f a@

-}
class (IsRHS n t f x) => (AppRHS n t f) x where
 type LeftRHS x a :: *
 appRHS :: RHS n t f (LeftRHS x a) -> x -> RHS n t f a

-- instance (Functor'RHS n t f, (t ~ String)) => AppRHS n t f String a where
--  type LeftRHS String a = a
--  appRHS f x = f <* toRHS x      --TODO inline apply definition, to remove functor constraint

-- instance (Functor'RHS n t f, (t ~ Text)) => AppRHS n t f Text a where
--  type LeftRHS Text a = a
--  appRHS f x = f <* toRHS x      --TODO inline apply definition, to remove functor constraint

instance (Functor'RHS n t f, (t ~ Text)) => AppRHS n t f String where
 type LeftRHS String a = a
 appRHS f x = f <* toRHS x      --TODO inline apply definition, to remove functor constraint

-- instance ((f' ~ f), Functor'RHS n t f) => AppRHS n t f' (RHS n t f x) a where
instance ((f' ~ f), (n' ~ n), Functor'RHS n t f) => AppRHS n' t f' (RHS n t f x) where
 type LeftRHS (RHS n t f x) a = (x -> a)
 appRHS f x = f <*> x --TODO inline apply definition, to remove functor constraint
-- the equality constraint (f' ~ f) delays unification, which (somehow) lets the abstract (z)'s in each (DNSEarleyFunc z a) unify.

-- | inject @a@s of different types into an @RHS@.
--
-- the first parameters (i.e. @n@ and @t@ and @f@) are always abstract;
-- they seem to be needed to be in scope to unify with themselves in @a@.
class IsRHS n t f r where
 type ToRHS r :: *
 toRHS :: r -> RHS n t f (ToRHS r)

instance (t ~ Text) => IsRHS n t f String where
  type ToRHS String = Text
  toRHS = Terminal id . T.pack

instance IsRHS n t f (RHS n t f a) where
  type ToRHS (RHS n t f a) = a
  toRHS = id
-- instance (IsString i) => IsRHS n t f String            where  type ToRHS String            = String;  toR = word . fromString

-- instance (t ~ String) => IsRHS n t f String where
--   type ToRHS String = String
--   toRHS = Terminal id

-- instance (t ~ Text) => IsRHS n t f Text where
--   type ToRHS Text = Text
--   toRHS = Terminal id


-- ================================================================ --

type DNSEarleyCommand z = Command
 (DNSEarleyName String)
 Text
 (DNSEarleyFunc z (DNSEarleyName String) Text)
 OSX.Application
 OSX.Actions_


-- ================================================================ --
-- RHS helpers

infix 2 <=>

(<=>) :: Name -> DNSEarleyRHS z a -> DNSEarleyRHS z a
-- type signature for type inference, disambiguates:
--  "No instance for (Data.String.IsString _)" and "No instance for (Functor _)"
(<=>) = genericGrammar

nonterminalGrammar :: String -> DNSEarleyRHS z a -> DNSEarleyRHS z a
nonterminalGrammar l r = NonTerminal (ConstName (defaultDNSInfo, l)) r

genericGrammar :: Name -> DNSEarleyRHS z a -> DNSEarleyRHS z a
genericGrammar name r = nonterminalGrammar (gui^.(guiIdentifier._Identifier)) r
 where Just gui = fromGlobalName name  -- TODO GHC 7.10.2 https://downloads.haskell.org/~ghc/7.10.2/docs/html/users_guide/other-type-extensions.html#special-implicit-params

-- | manually construct a special rule, with primitives.
simpleGrammar :: Name -> (E.Prod z String Text a) -> (DNSRHS Text Void) -> DNSEarleyRHS z a
simpleGrammar n p r = genericGrammar n $ liftLeaf p r

-- | manually construct a special rule, with two independent right-hand sides.
complexGrammar :: Name -> DNSEarleyRHS z a -> DNSEarleyRHS z x -> DNSEarleyRHS z a
complexGrammar n p r = genericGrammar n $ liftTree p r

-- | automatically generate a grammar from a type: the left-hand side comes from the type, and the right-hand side comes from the 'Show'n and transformed 'constructors'.
transformedGrammar :: forall z a. (Typeable a, Enum a, Show a) => (String -> String) -> DNSEarleyRHS z a
transformedGrammar f = nonterminalGrammar
 (guiOf(Proxy :: Proxy a) ^. (guiIdentifier._Identifier))  -- TODO Haskell type sections, whenever
 (asum . fmap (transformedCon f) $ constructors)

-- | helper function for conveniently using Dragon NaturallySpeaking built-ins; sets 'dnsInline' to true.
dragonGrammar :: Name -> (E.Prod z String Text a) -> DNSBuiltinRule -> DNSEarleyRHS z a
dragonGrammar name p r = set (_RHSInfo.dnsInline) True $ simpleGrammar name p (SomeDNSNonTerminal (DNSBuiltinRule r))

-- | a default 'Grammar' for 'Enum's.
--
-- with 'Enum's, we can get the "edit only once" property: edit the @data@ definition, then 'terminal' builds the 'Rule', and then the functions on 'Rule's build the 'Parser's and 'DNSGrammar's. with 'Typeable', but without TemplateHaskell.
--
-- the 'LHS' comes from the type, not the term (avoiding TemplateHaskell). other 'Grammar's can always be defined with an LHS that comes from the term, e.g. with '<=>' (as Haskell values' names are disjoint from Haskell types').
--
--
enumGrammar :: (Typeable a, Enum a, Show a) => DNSEarleyRHS z a
enumGrammar = transformedGrammar (overCamelCase id)

-- | a default 'Grammar' for simple ADTs.
--
-- detects the type name in a constructor name (as a
-- prefix/infix/suffix) and elides the affix.
--
-- useful when you want your @grammars@-DSL terminals to be
-- unqualified (for convenience), but you want your Haskell
-- identifiers to be qualified (to avoid conflicts). e.g.:
--
-- e.g. avoids naming conflicts with @Either@:
--
-- >>> :set -XDeriveDataTypeable
-- >>> data Button = LeftButton | ButtonMiddleButton | ButtonRight deriving (Show,Eq,Enum,Typeable)
-- >>> let button = qualifiedGrammar :: Grammar Button
-- >>> getWords . view gramGrammar $ button
-- ["left","middle","right"]
--
-- (the qualification is exaggerated to show the filtering behavior:
-- it's consistent in idiomatic declarations).
--
-- we didn't define @data Button = Left | Middle | Right@ because it
-- conflicts with 'Either', but the derived grammar is identical.
--
--
--
--
qualifiedGrammar :: forall z a. (Typeable a, Enum a, Show a) => DNSEarleyRHS z a
qualifiedGrammar = qualifiedGrammarWith occ
 where
 occ = guiOf(Proxy :: Proxy a) ^. (guiIdentifier._Identifier)
 -- GUI _ _ (Identifier occ) = guiOf (Proxy :: Proxy a)

-- | a default 'Grammar' for simple ADTs.
--
-- elides the given <http://en.wikipedia.org/wiki/Affix affix> from any part of any constructor.
--
-- e.g. avoids naming conflicts with @Either@. without making either the data type name too short, or the data constructor names too long:
--
-- >>> :set -XDeriveDataTypeable
-- >>> data Direction = UpD | DownD | LeftD | RightD  deriving (Show,Eq,Enum,Typeable)
-- >>> qualifiedGrammarWith "D" :: Grammar Direction
-- ["up","down","left","right"]
--
--
qualifiedGrammarWith :: (Typeable a, Enum a, Show a) => String -> DNSEarleyRHS z a
qualifiedGrammarWith affix = transformedGrammar (overCamelCase (filter (/= fmap toLower affix)))

-- | strips out data typename like 'qualifiedGrammar', and @_@'s, and numbers.
-- makes it easy to generate generic terminals (like @"left"@),
-- without conflicting with.common symbols (like 'Left').
tidyGrammar :: forall z a. (Typeable a, Enum a, Show a) => DNSEarleyRHS z a
tidyGrammar = transformedGrammar (overCamelCase (filter (/= fmap toLower occ)) . filter (/= '_'))
 where
 occ = guiOf(Proxy :: Proxy a) ^. (guiIdentifier._Identifier)

-- | a default 'Grammar' for 'String' @newtype@s.
--
-- the user might want to parse/recognize an arbitrary but dynamic/large subset of all possible strings.
-- For example:
--
-- * a mutable grammer whose contents depend on some context,
-- like the current buffer, or the previous recognition.
-- * a huge list of custom words, that sound like more common words,
-- that aren't being recognized, even after using Dragon's Vocabulary Builder.
-- * even a few static words, which don't need to be a sum typo,
-- to not increase boilerplate, while still increasing type safety.
--
-- e.g.
--
-- @
-- newtype Place = Place String deriving (Show,Eq)
-- instance IsString Place where fromString = Place
-- @
--
--
vocabularyGrammar :: [String] -> DNSEarleyRHS z Text
vocabularyGrammar = vocabulary

-- | the empty grammar. See 'UnitDNSRHS' (always matches, recognizing nothing) and 'unitEarleyParser' (always succeeds, parsing nothing).
epsilon :: DNSEarleyRHS z ()
epsilon = simpleGrammar 'epsilon unitEarleyParser UnitDNSRHS
 where unitEarleyParser = pure ()

--TODO generalize these introducers to any RHS, and use Text

word :: (IsString t, Show t) => String -> RHS n t f t
word = fromString

str :: String -> DNSEarleyRHS z Text
str = word

vocabulary :: (IsString t, Show t, Functor'RHS n t f) => [String] -> RHS n t f t
vocabulary = asum . fmap word

chr :: Char -> DNSEarleyRHS z Char
chr c = c <$ word [c]

-- | a specialization, @int = 'con'@, because integer literals are 'Num'-constrained polymorphic types.
-- in the context we will be using it, we need a concrete type for type inference.
int :: Int -> DNSEarleyRHS z Int
int = con

con :: (Show a) => a -> DNSEarleyRHS z a
con = transformedCon (List.intercalate " " . unCamelCase)

-- | make a 'Terminal' from the @transformed@ 'Show'n constructor, returning the constructor.
transformedCon :: (Show a) => (String -> String) -> a -> DNSEarleyRHS z a
transformedCon f x = x <$ (word . f . show $ x)

-- | @= 'optionRHS' 'enumDefault' ...@
optionalEnum :: (Enum a) => DNSEarleyRHS z a -> DNSEarleyRHS z a
optionalEnum = optionRHS enumDefault



-- ================================================================ --

{- | derive a parser from a DNSEarleyRHS, by observing sharing. 

("d" for DNS, "e" for Earley).

TODO safe with 'unsafePerformIO'?
-}
de'deriveParserObservedSharing :: DNSEarleyRHS (E.Rule s a) a -> ST s (E.Prod (E.Rule s a) String Text a)
de'deriveParserObservedSharing r1 = do
 r2 <- renameRHSToEarley >>= ($ r1)
 return$ induceEarley r2

{- | derive a grammar from a DNSEarleyRHS, by observing sharing. 

("d" for DNS, "e" for Earley).

-}
de'deriveGrammarObservedSharing :: DNSEarleyRHS z a -> IO SerializedGrammar
de'deriveGrammarObservedSharing rhs = do --TODO may throw exception 
 g <- formatRHS rhs >>= \case
  Right g  -> return g
  Left  es -> throwM$ DNSGrammarException es
 return g

newtype DNSGrammarException = DNSGrammarException [SomeException]
 deriving (Show)
deriving instance Exception DNSGrammarException



-- ================================================================ --

-- | the Earley parse function takes a Rank2 type (forall r. E.Prod r ...) that it instantiates to (forall s. (E.Rule s a)); then runST takes a Rank2 type (forall s. ...s...). this package exposes the internals of Earley, but of course not of ST. this type synonym is for convenience.
type RULED f s a = f (E.Rule s a) a
-- needs LiberalTypeSynonyms when f is a type synonym, I think.


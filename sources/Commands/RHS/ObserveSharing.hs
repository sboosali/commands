{-# LANGUAGE TypeOperators, RankNTypes, LambdaCase, TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-|
-}
module Commands.RHS.ObserveSharing where
-- import Commands.Extra
-- import Commands.RHS.Open
--
-- import Data.HFunctor.Recursion
-- import Data.HPrelude
-- import ReifiedBindings.HaskellBinding
--
-- --import System.Mem.StableName
-- import Data.Function
-- import Control.Monad.State
--
-- rhsObserveSharing :: RHS t HIdentity h :~> (IO :. (RHS t HaskellExpression1 h))
-- rhsObserveSharing = _rhsObserveSharing >>> (evalStateT&flip) ()
--
-- _rhsObserveSharing :: RHS t HIdentity h :~> (StateT () IO :. (RHS t HaskellExpression1 h))
-- _rhsObserveSharing r = do
--   k <- forceStableName r
--   s <- get
--   hlookup k s >>= \case
--     Just v  -> do
--       return v
--     Nothing -> mdo
--       () <- hmodify $ hinsert k v  -- use `v`
--       r' <- go r
--       v <- newHaskellExpression1 r'  -- make `v`
--       return r'
--
--   where
--   go = undefined
--   hlookup = undefined
--   hinsert = undefined
--   hmodify = undefined
--
-- _rhsfObserveSharing :: (HTraversable'RhsF n h) => RhsF t n h (RHS t HIdentity h) :~> (StateT () IO :. (RhsF t n h (RHS t HaskellExpression1 h)))
-- _rhsfObserveSharing = htraverseRhsF _rhsObserveSharing
--
--
-- os :: RHS t HIdentity h :~> (StateT () IO :. (RHS t HaskellExpression1 h))
-- os = ntraverseRhsF $ \(r,n) -> do
--   k <- forceStableName r
--   s <- get
--   hlookup k s >>= \case
--     Just v  -> do
--       return v
--     Nothing -> mdo
--       () <- hmodify $ hinsert k v  -- use `v`
--       r' <- go r
--       v <- newHaskellExpression1 r'  -- make `v`
--       return r'
--
--   where
--   go = undefined
--   hlookup = undefined
--   hinsert = undefined
--   hmodify = undefined
--

{-|

e.g.

@
production :: RHS t n h a -> RHS t HIdentity h a
production = NonTerminal . HIdentity

x = production $ ...
@


-}


--------------------------------------------------------------------------------

--TODO
-- type HaskellBinding1 = HIdentity HaskellBinding
-- type StableName1 = HIdentity StableName
-- type StableNameRHS t n h a = StableName (RHS t n h a)

--------------------------------------------------------------------------------


-- rhsObserveSharing = undefined
-- rhsObserveSharing = hapoM rhsObserveSharingF

-- rhsObserveSharingF :: RHS t HIdentity h :~> (IO :. (RHS t HaskellExpression1 h :+: RhsF t HaskellExpression1 h (RHS t HIdentity h)))
-- rhsObserveSharingF r = do
--   undefined

 -- Pure a -> Pure a
 -- rf `Apply` h -> u rf `Apply` (hfmap u h)
 -- rf :<*> rx -> u rf :<*> u rx

 -- Alter rs  -> Alter (u `map` rs)
 -- -- Opt  i x  -> i  <$> optional (go x)
 -- -- Many i x  -> i  <$> many     (go x)
 -- -- Some i x  -> i' <$> some     (go x) where i' = i . NonEmpty.fromList

 -- NonTerminal n -> NonTerminal (hfmap u n) -- TODO unshared
 -- Terminal i t -> Terminal i t

--old rhsObserveSharing :: RHS t n h a -> IO (RHS t (HaskellExpression1 :*: n) h a)
-- 
-- {-|
--
-- "higher", "endo", "para", "apo", "Monad".
--
-- a recursion scheme that:
--
-- * is top-down
-- * short-circuits
-- * is given the whole input
-- * is monadic
--
-- -}
-- hendoparapoM
--  :: ( h ~ HBase g
--     , HRecursive   g
--     , HCoRecursive g
--     , HTraversable h
--     , Monad m
--     )
--  => (f :~> (m :. h (g :+: f)))
--  -> (f :~> (m :. g))
-- hendoparapoM = hapoM


{-TODO

https://hackage.haskell.org/package/recursion-schemes-4.1.2/docs/Data-Functor-Foldable.html

http://hackage.haskell.org/package/dependent-map-0.2.1.0/docs/Data-Dependent-Map.html

https://hackage.haskell.org/package/stable-maps-0.0.5/docs/System-Mem-StableName-Dynamic.html

https://hackage.haskell.org/package/base-4.9.0.0/docs/System-Mem-StableName.html

recursion scheme:
- Top-down
- Short-circuits
- Monadic
- Input whole

- Top-down
"endo" ana

- Short-circuits
apo

- Monadic
cataM

- Input whole
para
(any CoRecursive Endo)

ana :: CoRecursive t => (a -> Base t a) -> a -> t

para :: Recursive t => (Base t (t, a) -> a) -> t -> a

apo :: CoRecursive t, Recursive t => (a -> Base t (Either t a)) -> a -> t

(f a -> m (Either (f a) (f a)))
Left means abort
Right means continue

\case
 NonTerminal

MonadIO m
MonadState (Map1 (StableName1 (RHS t n h)) (RHS t n h)) m

HMap key a = DMap (key a) a

HMap key f = DMap (key f) f
key f a :->? f a

MonadIO m
MonadState (HMap StableName1 (RHS t n h))

insert :: forall k f v. GCompare k => k v -> f v -> DMap k f -> DMap k f

NameMap = DMap StableName Identity

GCompare StableName1
StableName needs Ord
https://ghc.haskell.org/trac/ghc/ticket/913

module System.Mem.StableName
import GHC.Base ( StableName##, stableNameToInt# )

System.Mem.StableName.Map

(R -> m (Either R R))





HomogenizeM = State (Map n ...)
"transitive-closure" of "descendents-of-production"
 i.e. if a {NonTerminal n} is in a value, that {n} is a key
 TODO Liquid Haskell or dependent types?
 But,The benefit of direct reference is "Total lookups"

ReifyM = EitherT (Either NameError TypeError) IO
Fails on Type Error
uses StableName
 For both type-safety and name-safety
outputs HaskellName / HaskellBinding


share :: RHS Identity -> RHS StableName1
share :: RHS Identity -> RHS HaskellExpression1
share :: RHS n -> RHS (HaskellExpression1 :*: n)

isAcyclic : R n -> Maybe (R n)
isAcyclic : R n -> Either [Some n] (R n)
getStronglyConnectedComponents : R n -> Either NonEmpty (Some (R n)) (R n)
Ord n

-- all Sentences of a finite grammar
sentences : FiniteR t -> [Sentence t]
is streaming(?)
is efficient
bottom-up

efficient
sublinear
string builder
 Construction repeatedly appends, both left and right

bottom-up
F-algebra
 R0 n=[[t]] -> [[t]]
with caching on non-terminals?

hcata :: (HRecursive h, HFunctor (HBase h)) => HAlgebra (HBase h) f -> (h :~> f)
hcata algebra = hproject >>> hfmap (hcata algebra) >>> algebra

enumerateSentences :: FiniteGrammarF t n [Sentence t] -> [Sentence t]
cata \case
  FiniteTerminalF t      -> [Sentence [t]]
  FiniteOptionalF ts      -> Sentence [] : ts
  FiniteSequenceF ts1 ts2  -> (<>) <$> ts1 <*> ts2
  FiniteAlternativesF tss -> (concat . cross) tss --TODO too slow, generates millions On simple Grammar.


newtype HIdentity


hapoM coalgebra = (return <<< hinject) <=< htraverse (return <|||> hapoM coalgebra) <=< coalgebra

= coalgebra >=> htraverse (return <|||> hapoM coalgebra) >=> (hinject >>> return)

hapoM coalgebra = coalgebra >=> htraverse (return <|||> go) >=> (hinject >>> return)
where go = hapoM coalgebra


a mix of 'hapoM' and 'paraM', where @f ~ t@.

hendoparapoM
 :: ( h ~ HBase t
    , HRecursive   t
    , HCoRecursive t
    , HTraversable h
    , Monad m
    )
 => (t :~> (m :. h (t :+: t)))
 -> (t :~> (m :. t))
hendoparapoM = hapoM

NO

(t :*: t) is redundant!

and t changes type (Id to haskellExpression)

StableName (R HIdentity ..) ≠ StableName (R HaskellExpression1 ..)

RecursiveDo won't work
makeStableName before `evaluate` ≠ makeStableName after `evaluate`

RecursiveDo might work?
evaling the Constructor (to WHNF) terminates / is the same

: R n1 -> R n2


: RHS t n h :~> (IO :. (RHS t HaskellExpression1 h))

takes a grammar without `NonTerminal's (`n` is polymorphic)
gives a grammar with each haskell binding wrapped in `NonTerminal`


newHaskellExpression1 is strict
seq (not deepseq) each NonTerminal, as preprocess?


-}

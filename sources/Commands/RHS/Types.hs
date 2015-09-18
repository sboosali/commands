{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable, DeriveFunctor, FlexibleContexts, FlexibleInstances, GADTs #-}
{-# LANGUAGE KindSignatures, LambdaCase, LiberalTypeSynonyms           #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, PostfixOperators      #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, StandaloneDeriving       #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators              #-}
module Commands.RHS.Types where
-- import           Commands.Etc

import           Control.Lens
import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty  as NonEmpty

import           Control.Applicative
import           Data.Foldable       (asum)
import           Data.Monoid
import           GHC.Exts            (IsList (..), IsString (..))


data RHS n t f a where

 -- Applicative instance
 Pure        :: a                                     -> RHS n t f a
 Apply       :: (RHS n t f (x -> a)) -> (f x)         -> RHS n t f a
 (:<*>)      :: (RHS n t f (x -> a)) -> (RHS n t f x) -> RHS n t f a

 -- Alternative/Monoid instance
 Alter       :: ![RHS n t f a] -> RHS n t f a

 -- (Coyoneda'd?) methods
 Opt         :: !(Maybe x    -> a) -> RHS n t f x -> RHS n t f a
 Many        :: !([x]        -> a) -> RHS n t f x -> RHS n t f a
 Some        :: !(NonEmpty x -> a) -> RHS n t f x -> RHS n t f a

 -- (Coyoneda'd?) grammar-specific stuff
 Terminal    :: !(t -> a)  -> !t          -> RHS n t f a
 NonTerminal :: !(n t f a) -> RHS n t f a -> RHS n t f a

pattern Empty = Alter []

-- type RHSFunctorC n t f = (Functor f, Functor (n t f)) ConstraintKinds

-- TODO expects constraint:
deriving instance (Functor (n t f)) => (Functor (RHS n t f))
-- deriving instance () => Data (RHS n t f a)

instance (Functor f, Functor (n t f)) => Monoid (RHS n t f a) where
 mempty = Empty
 mappend = (<|>)

instance (Functor f, Functor (n t f)) => Applicative (RHS n t f) where
 pure = Pure

 Pure xa <*> tx                = fmap xa tx                       -- Functor
 -- Pure {id} <*> x            = fmap {id} x     = x              -- Identity
 -- Pure f <*> Pure x          = fmap f (Pure x) = Pure (f x)     -- Homomorphism
 txa     <*> Pure x            = fmap ($ x) txa                   -- Interchange

 Empty   <*> _                 = Empty                            -- left-Annihilation (?)
 _       <*> Empty             = Empty                            -- right-Annihilation
 txa     <*> Alter txs         = txa :<*> Alter txs               -- NO left-Distributivity

 txa     <*> (tyx `Apply` fy)  = ((.) <$> txa <*> tyx) `Apply` fy -- Composition
 txa     <*> (tyx :<*>    ty)  = ((.) <$> txa <*> tyx) :<*>    ty -- Composition

 txa     <*> (Opt  ysa ty)     = txa :<*> Opt  ysa ty -- TODO correct?
 txa     <*> (Many ysa ty)     = txa :<*> Many ysa ty
 txa     <*> (Some ysa ty)     = txa :<*> Some ysa ty

 txa     <*> (Terminal    i t)   = txa :<*> Terminal    i t -- TODO correct?
 txa     <*> nx@NonTerminal{} = txa :<*> nx -- TODO to preserve sharing?
-- https://hackage.haskell.org/package/Earley-0.8.3/docs/src/Text-Earley-Grammar.html#line-85

instance (Functor f, Functor (n t f)) => Alternative (RHS n t f) where
 empty = Empty

 Empty <|> y = y                            -- Left-Identity
 x <|> Empty = x                            -- Right-Identity
 x <|> y = Alter (toRHSList x <> toRHSList y) -- Associativity
 {-# INLINE (<|>) #-}

 many = Many id
 {-# INLINE many #-}
 some = fmap NonEmpty.toList . Some id
 {-# INLINE some #-}

toRHSList :: RHS n t f a -> [RHS n t f a]
toRHSList (Alter xs) = xs
toRHSList x = [x]
{-# INLINE toRHSList #-}

-- |
liftRHS :: f a -> RHS n t f a
liftRHS f = Pure id `Apply` f
{-# INLINE liftRHS #-}

{- | both token and result must be an (instance of) 'IsString'.

(see <http://chrisdone.com/posts/haskell-constraint-trick the constraint trick>)

@t@ can default to String.

-}
instance (IsString t, Show t, a ~ t) => IsString (RHS n t f a) where --TODO remove Show constraint or show it's needed for defaulting
 fromString s = Terminal id t where t = fromString s
-- instance (IsString t, Show t, a ~ t) => IsString (RHS n t f a) where fromString = Terminal id . fromString
-- instance (IsString t, Show t, a ~ String) => IsString (RHS n t f a) where fromString = Terminal show . fromString
-- instance (IsString t, Show t) => IsString (RHS n t f String) where fromString = Terminal show . fromString
-- instance (IsString t) => IsString (RHS n String f t) where fromString = Terminal fromString
-- instance (IsString t, Show t) => IsString (RHS n t f t) where fromString = Terminal id . fromString

-- | @([r1,r2,r3] :: RHS n t f a)@ is @('mconcat' [r1,r2,r3])@ is @('asum' [r1,r2,r3])@ is @(r1 '<|>' r2 '<|>' r3)@
instance IsList (RHS n t f a) where
 type Item (RHS n t f a) = RHS n t f a
 fromList = Alter             -- the constructor (rather than a method like "asum") avoids the (Functor f) constraint
 toList = toRHSList

terminal :: t -> RHS n t f t
terminal = Terminal id

-- | @(-?) = 'optionalRHS'@
(-?), optionalRHS :: RHS n t f a -> RHS n t f (Maybe a)
(-?) = optionalRHS
optionalRHS = Opt id

-- | @(-?-) = 'flip' 'optionRHS'@
(-?-) :: RHS n t f a -> a -> RHS n t f a
(-?-) = flip optionRHS
optionRHS :: a -> RHS n t f a -> RHS n t f a
optionRHS x = Opt (maybe x id)

-- | @(-*) = 'manyRHS'@
(-*), manyRHS :: RHS n t f a -> RHS n t f [a]
(-*) = manyRHS
manyRHS = Many id

-- | @(-+) = 'someRHS'@
(-+), someRHS :: RHS n t f a -> RHS n t f (NonEmpty a)
(-+) = someRHS
someRHS = Some id

-- | @(-++) = 'many1RHS'@
--
-- like 'someRHS', but "downcasted" to a list.
(-++), many1RHS :: RHS n t f a -> RHS n t f [a]
(-++) = many1RHS
many1RHS = Some NonEmpty.toList

-- | @(-|-) = 'eitherRHS'@
--
-- a heterogeneous binary 'Alter'.
(-|-), eitherRHS :: RHS n t f a -> RHS n t f b -> RHS n t f (Either a b)
(-|-) = eitherRHS
eitherRHS l r = Alter [Pure Left :<*> l, Pure Right :<*> r] -- constructors avoid Functor constraint 
-- eitherRHS l r = (Left <$> l) <|> (Right <$> r)

(-#-) :: (Functor f, Functor (n t f)) => Int -> RHS n t f a -> RHS n t f [a]
(-#-) k = traverse id . replicate k


runRHS
 :: forall n t f g a. (Alternative g)
 => (forall x. n t f x -> RHS n t f x -> g x)
 -> (          t                      -> g t)
 -> (forall x. f x                    -> g x)
 -> RHS n t f a
 -> g a

runRHS fromN fromT fromF = \case
 Terminal    i t -> i <$> fromT t
 NonTerminal n r ->       fromN n r

 Opt  i x  -> i  <$> optional (go x)
 Many i x  -> i  <$> many     (go x)
 Some i x  -> i' <$> some     (go x) where i' = i . NonEmpty.fromList

 Pure a      -> pure a
 f `Apply` x -> go f <*> fromF x
 f :<*>    g -> go f <*> go g
 Alter fs  -> asum (go `map` fs)

 where
 go :: forall x. RHS n t f x -> g x
 go = runRHS fromN fromT fromF

-- | An "unlifted" 'runRHS'. ignores value (in Pure) and transformations in (Terminal/Opt/Many/Some).
--
-- The arguments: @foldRHS fromN fromT fromF unit mul add opt_ many_ some_@
foldRHS
 :: forall n t f a b. ()
 => (forall x. n t f x -> b -> b)
 -> (          t                      -> b)
 -> (forall x. f x                    -> b)
 -> b
 -> (b -> b -> b)
 -> ([b] -> b)
 -> (b -> b)
 -> (b -> b)
 -> (b -> b)
 -> RHS n t f a
 -> b
foldRHS fromN fromT fromF unit mul add opt_ many_ some_ = \case
 Terminal    _ t -> fromT t
 NonTerminal n r -> fromN n (go r)
 Opt  _ x  -> opt_  (go x)
 Many _ x  -> many_ (go x)
 Some _ x  -> some_ (go x)
 Pure _      -> unit
 f `Apply` x -> go f `mul` fromF x
 f :<*>    g -> go f `mul` go g
 Alter fs  -> add (go `map` fs)
 where
 go :: forall x. RHS n t f x -> b
 go = foldRHS fromN fromT fromF unit mul add opt_ many_ some_

{- | unwraps a 'NonTerminal', otherwise preserves the input.
when NonTerminals are interpreted as tagging a right-hand side with a left-hand side for "sharing", this function should "unshare" its input from other right-hand side expressions with the same "name".
(the double underscores at weirdness)
-}
__inlineRHS__ :: RHS n t f a -> RHS n t f a
__inlineRHS__ = \case
 NonTerminal _ r -> r
 r -> r

-- a Traversal?
renameRHS'
 :: forall m n1 n2 t f a. (Applicative m)
 => (forall x. RHS n1 t f x ->     n1 t f x -> m (    n2 t f x))
 -- => (forall x. RHS n1 t f x ->     n1 t f x -> RHS n1 t f x -> m (    n2 t f x , RHS n2 t f x))
 -> (                          RHS n1 t f a ->                 m (RHS n2 t f a))
renameRHS' u = \case
 k@(NonTerminal x r)  ->  NonTerminal <$> u k x <*> go r -- like traverse, except this case
 -- k@(NonTerminal x r)  ->  (uncurry NonTerminal) <$> u k x r
 Terminal i r       ->  pure$ Terminal i r
 Opt  i r           ->  Opt  i <$> go r
 Many i r           ->  Many i <$> go r
 Some i r           ->  Some i <$> go r
 Pure a             ->  pure$ Pure a
 r `Apply` x        ->  Apply  <$> go r <*> pure x -- preserved
 r :<*> r'          ->  (:<*>) <$> go r <*> go r'
 Alter rs           ->  Alter <$> go `traverse` rs
 where
 go :: forall x. RHS n1 t f x -> m (RHS n2 t f x)
 go = renameRHS' u

renameRHS
 :: forall m n1 n2 t f a. (Applicative m)
 => (forall x. RHS n1 t f x ->     n1 t f x -> RHS n1 t f x -> m (    n2 t f x , RHS n2 t f x))
 -> (                          RHS n1 t f a ->                 m (RHS n2 t f a))
renameRHS u = \case
 k@(NonTerminal x r)  ->  uncurry(NonTerminal) <$> u k x r
 Terminal i r       ->  pure$ Terminal i r
 Opt  i r           ->  Opt  i <$> go r
 Many i r           ->  Many i <$> go r
 Some i r           ->  Some i <$> go r
 Pure a             ->  pure$ Pure a
 r `Apply` x        ->  Apply  <$> go r <*> pure x -- preserved
 r :<*> r'          ->  (:<*>) <$> go r <*> go r'
 Alter rs           ->  Alter <$> go `traverse` rs
 where
 go :: forall x. RHS n1 t f x -> m (RHS n2 t f x)
 go = renameRHS u

-- | e.g. @('RHS' (ConstName n) t f a)@
data ConstName n t (f :: * -> *) a = ConstName { _unConstName :: !n } deriving (Functor)
-- KindSignatures because: f being phantom, it's kind is inferred to be nullary (I think)
-- TODO is PolyKinds better? (f :: k)

data SomeRHS n t f = SomeRHS { _unSomeRHS :: forall x. RHS n t f x }

-- -- | the children
-- selfRHS :: Traversal (RHS n t f a) (RHS n' t' f' a')
-- selfRHS = traverse

-- invmapTerminal :: (t1 -> t2) -> (t2 -> t1) -> RHS n t1 f a -> RHS n t2 f a
-- invmapTerminal fromT intoT = \case
--  Terminal i t -> Terminal (i.intoT) (fromT t)
--  r ->

data Command n t f c b a = Command
 { _cRHS     :: RHS n t f a         -- ^ the root of a grammar
 , _cBest    :: NonEmpty a -> a    -- ^ for disambiguating multiple parse results
 , _cDesugar :: c -> a -> b    -- ^ "desugar" the parse result into "bytecode" actions
 }

-- instance ((f' ~ f), (n' ~ n), Functor'RHS n t f) => AppRHS n' t f' (Command n t f c b x) where
--  type LeftRHS (Command n t f c b x) a = (x -> a)
--  appRHS f x = f <*> toRHS x

-- instance IsRHS n t f (Command n t f c b a) where
--   type ToRHS (Command n t f c b a) = a
--   toRHS = _cRHS


-- ================================================================ --
-- lenses

_NonTerminal :: Prism' (RHS n t f a) (n t f a, RHS n t f a)
_NonTerminal = prism (uncurry NonTerminal) $ \case
 NonTerminal l r -> Right (l, r)
 r -> Left r
-- makePrisms ''RHS
makeLenses ''ConstName
makeLenses ''SomeRHS
makeLenses ''Command

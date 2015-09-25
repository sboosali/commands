{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable, DeriveFunctor, FlexibleContexts, FlexibleInstances, GADTs #-}
{-# LANGUAGE KindSignatures, LambdaCase, LiberalTypeSynonyms           #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, PostfixOperators      #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, StandaloneDeriving       #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fno-warn-missing-signatures  #-}
module Commands.RHS.Types where
-- import           Commands.Etc

import           Control.Lens
import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty  as NonEmpty

import qualified Data.List as List 
import           Control.Applicative
import           Data.Foldable       (asum)
import           Data.Monoid
import           GHC.Exts            (IsList (..), IsString (..))


data RHS n t f a where

 Pure        :: a                                     -> RHS n t f a -- Applicative 'pure'
 Apply       :: (RHS n t f (x -> a)) -> (f x)         -> RHS n t f a -- Applicative '<*>'
 (:<*>)      :: (RHS n t f (x -> a)) -> (RHS n t f x) -> RHS n t f a -- Applicative '<*>'

 Alter       :: [RHS n t f a]                    -> RHS n t f a -- Alternative '<|>' / Monoid '<>'  
 Opt         :: (Maybe x    -> a) -> RHS n t f x -> RHS n t f a -- Alternative 'optional'
 Many        :: ([x]        -> a) -> RHS n t f x -> RHS n t f a -- Alternative 'many'
 Some        :: (NonEmpty x -> a) -> RHS n t f x -> RHS n t f a -- Alternative 'some'

 -- grammar-specific stuff
 Terminal     :: (t -> a)  -> !t          -> RHS n t f a   -- grammatical terminal symbol (Coyoneda'd)
 NonTerminal  :: (n t f a) -> RHS n t f a -> RHS n t f a   -- grammatical non-terminal symbol
 Terminals    :: (t -> a)               -> RHS n t f a -- a placeholder for a set of terminals (e.g. set of all terminal symbols in the grammar. see 'getTerminals')

-- | @pattern Empty = Alter []@
pattern Empty = Alter []

-- type RHSFunctorC n t f = (Functor f, Functor (n t f)) ConstraintKinds

deriving instance (Functor (n t f)) => (Functor (RHS n t f)) -- TODO expects constraint:
-- deriving instance () => Data (RHS n t f a)

-- | lawful (coincides with 'Alternative' instance)
instance (Functor f, Functor (n t f)) => Monoid (RHS n t f a) where
 mempty = Empty
 mappend = (<|>)

{- | mostly lawful. 'fmap' and 'pure' behave lawfully. 

left-distributivity of '<*>' over '<|>' is intentionally violated. that is, we want @(x <|> y) <*> z@ to be preserved, not to be distributed into @(x <*> z) <|> (y <*> z)@. this helps: 

* when @(x <|> y)@ is actually the infinite @(x <|> y <|> ...)@, interpreting the undistributed @(x <|> y) <*> z@ might terminate while the @(x <|> y <|> ...) <*> z@ may terminate while the distributed @(x <*> z) <|> (y <*> z) <|> ...@ will not.
* when the interpretation (e.g. a chart parser) can increase performance by sharing such "inner alternation". 

'<*>' is left-associated. 

-}
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

 txa     <*> (Opt  ysa ty)     = txa :<*> Opt  ysa ty -- NOTE doesn't distribute, intentionally 
 txa     <*> (Many ysa ty)     = txa :<*> Many ysa ty -- NOTE doesn't distribute, intentionally 
 txa     <*> (Some ysa ty)     = txa :<*> Some ysa ty -- NOTE doesn't distribute, intentionally 

 txa     <*> (Terminal i t)   = txa :<*> Terminal    i t -- TODO correct?
 txa     <*> nx@NonTerminal{} = txa :<*> nx -- TODO to preserve sharing?
 txa     <*> Terminals i    = txa :<*> Terminals i -- NOTE greatly simplifies "self-referential" grammars (self-recursive grammars are already simple)

-- https://hackage.haskell.org/package/Earley-0.8.3/docs/src/Text-Earley-Grammar.html#line-85

-- | lawful.  
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



-- ================================================================ --

terminal :: t -> RHS n t f t
terminal = Terminal id

terminals :: RHS n t f t
terminals = Terminals id

-- | zero or one. @(-?) = 'optionalRHS'@
(-?), optionalRHS :: RHS n t f a -> RHS n t f (Maybe a)
(-?) = optionalRHS
optionalRHS = Opt id -- NOTE constructors (rather than methods) avoid Functor constraint 

-- | zero or one. @(-?-) = 'flip' 'optionRHS'@
(-?-) :: RHS n t f a -> a -> RHS n t f a
(-?-) = flip optionRHS
optionRHS :: a -> RHS n t f a -> RHS n t f a
optionRHS x = Opt (maybe x id) -- NOTE constructors (rather than methods) avoid Functor constraint 

-- | zero or more. @(-*) = 'manyRHS'@
(-*), manyRHS :: RHS n t f a -> RHS n t f [a]
(-*) = manyRHS
manyRHS = Many id -- NOTE constructors (rather than methods) avoid Functor constraint 

-- | one or more. @(-+) = 'someRHS'@
(-+), someRHS :: RHS n t f a -> RHS n t f (NonEmpty a)
(-+) = someRHS
someRHS = Some id -- NOTE constructors (rather than methods) avoid Functor constraint 

-- | one or more. @(-++) = 'many1RHS'@
--
-- like 'someRHS', but "downcasted" to a list.
(-++), many1RHS :: RHS n t f a -> RHS n t f [a]
(-++) = many1RHS
many1RHS = Some NonEmpty.toList -- NOTE constructors (rather than methods) avoid Functor constraint 

-- | @(-|-) = 'eitherRHS'@
--
-- a heterogeneous binary 'Alter'.
(-|-), eitherRHS :: RHS n t f a -> RHS n t f b -> RHS n t f (Either a b)
(-|-) = eitherRHS
eitherRHS l r = Alter [Pure Left :<*> l, Pure Right :<*> r] -- NOTE constructors (rather than methods) avoid Functor constraint
-- thanks to quasi-lawfulness, about the same as {{eitherRHS l r = (Left <$> l) <|> (Right <$> r)}} 

-- (-#-) :: (Functor f, Functor (n t f)) => Int -> RHS n t f a -> RHS n t f [a]
-- (-#-) k = traverse id . replicate k -- TODO what is this



-- ================================================================ --

-- | ignores f and n (which may be mutually recursive with the whole) 
getTerminals :: (Eq t) => RHS n t f a -> [t]
getTerminals = getTerminalsNF (const id) (const [])

-- TODO take the traversal from either the non-terminal or the functor into the right-hand side again. finds any hidden children right-hand sides. 
-- TODO always terminates, even on recursive grammars 
getTerminalsNF
 :: (Eq t)
 => (forall x. n t f x -> [t] -> [t])
 -> (forall x. f x          -> [t])
 -> RHS n t f a
 -> [t]
getTerminalsNF fromN fromF = List.nub . foldRHS' fromN (:[]) fromF [] [] (<>) concat id id id

runRHS
 :: forall n t f g a. (Alternative g, Eq t)
 => (forall x. n t f x -> RHS n t f x -> g x)
 -> (          t                      -> g t)
 -> (forall x. f x                    -> g x)
 -- -> (          [t]                    -> g [t])
 -> RHS n t f a
 -> g a
runRHS fromN fromT fromF rhs
 = runRHS' fromN fromT fromF ((asum . map fromT) (getTerminals rhs)) rhs 

runRHS'
 :: forall n t f g a. (Alternative g, Eq t)
 => (forall x. n t f x -> RHS n t f x -> g x)
 -> (          t                      -> g t)
 -> (forall x. f x                    -> g x)
 -> g t -- TODO
 -> RHS n t f a
 -> g a

runRHS' fromN fromT fromF aTerminal = \case
 Terminals i -> i <$> aTerminal 
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
 go = runRHS' fromN fromT fromF aTerminal 

foldRHS
 :: forall n t f a b. (Eq t)
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
foldRHS fromN fromT fromF unit mul add opt_ many_ some_ rhs
 = foldRHS' fromN fromT fromF ((add . map fromT) (getTerminals rhs)) unit mul add opt_ many_ some_ rhs 

-- | An "unlifted" 'runRHS'. ignores value (in Pure) and transformations in (Terminal/Opt/Many/Some).
--
-- The arguments: @foldRHS fromN fromT fromF unit mul add opt_ many_ some_@
foldRHS'
 :: forall n t f a b. 
    (forall x. n t f x -> b -> b)
 -> (          t                      -> b)
 -> (forall x. f x                    -> b)
 -> b  -- TODO
 -> b
 -> (b -> b -> b)
 -> ([b] -> b)
 -> (b -> b)
 -> (b -> b)
 -> (b -> b)
 -> RHS n t f a
 -> b
foldRHS' fromN fromT fromF aTerminal unit mul add opt_ many_ some_ = \case
 Terminals _ -> aTerminal 
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
 go = foldRHS' fromN fromT fromF aTerminal unit mul add opt_ many_ some_

{- | unwraps a 'NonTerminal', otherwise preserves the input.

when NonTerminals are interpreted as tagging a right-hand side with a left-hand side for "sharing", this function should "unshare" its input from other right-hand side expressions with the same "name".

(the double underscores imply magic; while this function is a magical, it's useful with magical interpretations like observed sharing)

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
 Terminals i        ->  pure$ Terminals i 
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
 Terminals i        ->  pure$ Terminals i 
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



-- ================================================================ --

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

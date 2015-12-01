{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, KindSignatures, StandaloneDeriving, RankNTypes, UndecidableInstances  #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PatternSynonyms, TypeOperators, ConstraintKinds, TypeFamilies, GeneralizedNewtypeDeriving   #-}
module Commands.RHS.Open where
import Data.HFunctor.Recursion
import Data.HPrelude 

-- import Data.List.NonEmpty (NonEmpty) 
-- import qualified Data.List.NonEmpty as NonEmpty 

-- import Control.Monad.Trans.State
import           Control.Applicative
import Data.Void (Void)  
import           Data.Monoid
import           GHC.Exts            (IsList (..), IsString (..))
-- import System.Mem.StableName


{- | a recursive 'RhsF'. 

note on types: 

* @n@ and @h@ are higher-kinded type constructors 
(TODO they should know about all four type parameters, including each other). 

* @r@ is "fixed" to @(RHS t n h a)@ itself

isomorphic to @HFix (RhsF t n h)@, but the fixpoint is specialized because: 

* @newtype@s provide better error messages than @type@s, and the user sees this type. 
* (more importantly) we can define domain-specific 'Applicative'/'Alternative' instances. 

-}
newtype RHS t n h a = RHS { unRHS :: RhsF
 (t)
 (n)
 (h) 
 (RHS t n h)
 (a)
 }


deriving instance (Functor'RHS t n h) => (Functor (RHS t n h)) -- NOTE uses UndecidableInstances


-- deriving instance () => Data (RHS t n h a)


{- | mostly lawful: 'fmap' and 'pure' behave lawfully. 

left-distributivity of '<*>' over '<|>' is intentionally violated. that is, we want @(x \<|> y) \<*> z@ to be preserved, not to be distributed into @(x \<*> z) \<|> (y \<*> z)@. this helps: 

* when @(x \<|> y)@ is actually the infinite @(x \<|> y \<|> ...)@; interpreting the undistributed @(x \<|> y) \<*> z@ might terminate while the @(x \<|> y \<|> ...) \<*> z@ may terminate while the distributed @(x \<*> z) \<|> (y \<*> z) \<|> ...@ will not.
* when the interpretation (e.g. a chart parser) can increase performance by sharing such "inner alternation". 

'<*>' is left-associated. 

-}
instance (Functor'RHS t n h) => Applicative (RHS t n h) where
 {-# INLINEABLE pure #-} 
 pure = RHS . Pure

 {-# INLINEABLE (<*>) #-} 

 RHS (Pure xa) <*> RHS rx                = RHS$ fmap xa rx        -- Functor
 -- Pure {id} <*> x            = fmap {id} x     = x              -- Identity
 -- Pure f <*> Pure x          = fmap f (Pure x) = Pure (f x)     -- Homomorphism

 RHS Empty    <*> _                  = RHS Empty                            -- left-Annihilation (?)
 _            <*> RHS Empty          = RHS Empty                            -- right-Annihilation

 RHS rxa <*> rx = RHS$ case rx of
  RHS (Pure x)         -> fmap ($ x) rxa                       -- Interchange
  RHS (ryx `Apply` hy) -> ((.) <$> RHS rxa <*> ryx) `Apply` hy -- Composition
  RHS (ryx :<*>    ry) -> ((.) <$> RHS rxa <*> ryx) :<*>    ry -- Composition
  RHS (Alter _rxs)     -> RHS rxa :<*> rx                      -- NO left-Distributivity

 --  r@(Opt  _ysa _ty)     = RHS$ txa :<*> RHS r    -- NOTE intentionally doesn't distribute
 --  r@(Many _ysa _ty)     = RHS$ txa :<*> RHS r    -- NOTE intentionally doesn't distribute
 --  r@(Some _ysa _ty)     = RHS$ txa :<*> RHS r    -- NOTE intentionally doesn't distribute

  RHS (NonTerminal{}) -> RHS rxa :<*> rx -- NOTE preserving sharing is critical for the observed-sharing interface 
  RHS (Terminal   {}) -> RHS rxa :<*> rx 
 -- txa     <*> r@(Terminals _i)      = RHS$ txa :<*> r -- NOTE greatly simplifies "self-referential" grammars (self-recursive grammars are already simple)


-- | lawful (coincides with 'Monoid' instance).  
instance (Functor'RHS t n h) => Alternative (RHS t n h) where
 empty = mempty
 (<|>) = mappend

 -- many = ManyRHS id
 -- {-# INLINE many #-}

 -- some = fmap NonEmpty.toList . SomeRHS id
 -- {-# INLINE some #-}


-- | lawful.  
instance (Functor'RHS t n h) => Monoid (RHS t n h a) where
 mempty = EmptyRHS

 EmptyRHS `mappend` y        = y                                     -- Left-Identity
 x        `mappend` EmptyRHS = x                                     -- Right-Identity
 x        `mappend` y        = AlterRHS (toListRHS x <> toListRHS y) -- Associativity
 {-# INLINE mappend #-}


-- | @([r1,r2,r3] :: RHS t n h a)@ is @('mconcat' [r1,r2,r3])@ is @('asum' [r1,r2,r3])@ is @(r1 '<|>' r2 '<|>' r3)@
instance IsList (RHS t n h a) where
 type Item (RHS t n h a) = (RHS t n h a)
 fromList = AlterRHS             -- the constructor (rather than a method like "asum") avoids the (Functor f) constraint
 toList = toListRHS


type instance HBase (RHS t n h) = RhsF t n h 

instance (HFunctor h) => HRecursive (RHS t n h) where hproject = unRHS

instance (HFunctor h) => HCoRecursive (RHS t n h) where hinject = RHS


-- | @ConstraintKinds@
type Functor'RHS t n h = Functor (h (RHS t n h))

-- | @pattern EmptyRHS = 'RHS' ('Alter' [])@
pattern EmptyRHS = RHS (Alter []) -- TODO AlterF 

-- | @pattern AlterRHS rs = 'RHS' ('Alter' rs)@
pattern AlterRHS rs = RHS (Alter rs) 

-- -- | @pattern ManyRHS r = 'RHS' ('Many' r)@
-- pattern ManyRHS r = RHS (Many r) 

-- -- | @pattern SomeRHS r = 'RHS' ('Some' r)@
-- pattern SomeRHS r = RHS (Some r) 


-- ================================================================ --

{-| a non-recursive and lower-order 'RhsF'. 

note on types: 

* @Void@          knocks out the 'NonTerminal' constructor, since @0 * a = 0@. 
* @(HConst f)@    lifts the given lower-order functor.  
* @(Const n)@     direct reference becomes indirect reference.  
* @()@            trivializes the injections, since @1^a = a@.   

-}
newtype RHS_ t n f = RHS_ { unRHS_ :: RhsF -- TODO naming: RHS0?  
 (t)
 (Void)
 (HConst f)
 (Const n)
 ()
 }


{-| 

-}
-- type ReifiedRHS t n f = Map n (RHS_ t n f)


-- {-| 

-- -}
-- reifyRHS :: (Ord n) => RHS t n (HConst f) a -> ReifiedRHS t n f 
-- reifyRHS = flip(execState) [] . hapoM coalgebra
--  where 
-- (f :~> (m :. h (t :+: f)))
--  coalgebra :: (Ord n) => RHS t n (HConst f) :~> (State (ReifiedRHS t n f) :. (RhsF t n (HConst f) (RHS t n (HConst f) :+: ())))
--  coalgebra = \case 
--   NonTerminal n r -> do
--    Map.insert n r
--   _ -> return ()



-- ================================================================ --

{- | a right-hand side in a
<https://en.wikipedia.org/wiki/Extended_Backus-Naur_Form EBNF>.

EBNF features (using Wikipedia's jargon):

* "terminals" come from 'Terminal' / 'Terminals'. 
* "alternation" comes from the 'Alternative' instance, via 'Alter'.
* "sequencing" comes from 'Applicative' instance, via 'Pure' / 'Apply' / ':<*>'.
* "optionality" and "repetition" come from 'Opt' / 'Many' / 'Some'.
* "grouping" just comes from Haskell's precedence/parentheses.
* *no* "exceptions"
* "non-terminals" come from 'NonTerminal' 
(and thus, the name "RHS" is a bit of a lie, as this one case can hold a "LHS" too). 

also, it's a <http://bnfc.digitalgrammars.com/ labeled BNF>:

* a constructor on the left of '<$>' labels the grammatical sequence.
* the type of '<*>' is heterogeneous, which lets us capture right-hand-sides of different types

naming: 

* @t@: "terminal" or "token"  
* @n@: "non-terminal" or "name"  
* @h@: "higher-order" functor 
* @r@: "right-hand-side" or "recursive"  
* @a@: "any" value

the @h@ parameter provides extensibility. 
an 'RhsF' and its @h@ may be mutually recursive, and @h@ should also be a "fixed-point functor". 
the recursion schemes in this package are defined with the above two things in mind. 
e.g.:

TODO 

when partially applied, @RhsF t n h@ and its @f@ will have the same kind-arity: 

@
RhsF t n h        :: ((* -> *) -> (* -> *)) -> (* -> *)
         h        :: ((* -> *) -> (* -> *)) -> (* -> *)

HFix           h  :: * -> *
HFix (RhsF t n h) :: * -> *

RhsF t n h r      :: * -> *
         h r      :: * -> *

RhsF t n h r a    :: *
         h r a    :: *
@

-}
data RhsF
 :: *
 -> *
 -> ((* -> *) -> (* -> *))
 -> (* -> *)
 -> (* -> *)
 where

 Pure        :: a                           -> RhsF t n h r a
 Apply       :: (r (x -> a)) -> (h r x)     -> RhsF t n h r a
 (:<*>)      :: (r (x -> a)) -> (r x)       -> RhsF t n h r a

 Alter       :: [r a]                    -> RhsF t n h r a
 -- Opt         :: (Maybe x    -> a) -> r x -> RhsF t n h r a
 -- Many        :: ([x]        -> a) -> r x -> RhsF t n h r a
 -- Some        :: (NonEmpty x -> a) -> r x -> RhsF t n h r a

 NonTerminal :: n          -> r a -> RhsF t n h r a -- TODO one field?  :: n -> RhsF t n h r a  TODO move into RHS?  
 Terminal    :: (t -> a)   -> !t  -> RhsF t n h r a         
 -- Terminals   :: (t -> a)                         -> RhsF t n h r a  -- placeholder for the set of terminals in the grammar 

deriving instance (Functor'RhsF h r) => (Functor (RhsF t n h r))


instance (HFunctor h) => HFunctor (RhsF t n h) where
 hfmap = hfmapRhsF 
 {-# INLINEABLE hfmap #-}


-- instance (HFoldable h) => HFoldable (RhsF t n h) where
--  hfoldMap = hfoldMapRhsF 
--  {-# INLINEABLE hfold #-}


instance (HTraversable h) => HTraversable (RhsF t n h) where
 htraverse = htraverseRhsF 
 {-# INLINEABLE htraverse #-}


{- | both token and result must be an (instance of) 'IsString'.

(see <http://chrisdone.com/posts/haskell-constraint-trick the constraint trick>)

@t@ can default to String.

-}
instance (IsString t, Show t, a ~ t) => IsString (RhsF t n h r a) where --TODO remove Show constraint or show it's needed for defaulting
 fromString s = Terminal id t where t = fromString s
 {-# INLINEABLE fromString #-}


-- -- | @([r1,r2,r3] :: RHS t n h a)@ is @('mconcat' [r1,r2,r3])@ is @('asum' [r1,r2,r3])@ is @(r1 '<|>' r2 '<|>' r3)@
-- instance IsList (RhsF t n h r a) where
--  type Item (RhsF t n h r a) = (RHS t n h a)
--  fromList = Alter             -- the constructor (rather than a method like "asum") avoids the (Functor f) constraint
--  toList = toListRHS


-- | @ConstraintKinds@
type Functor'RhsF h r =
 ( Functor r
 , Functor (h r)
 )


-- | @pattern Empty = 'Alter' []@
pattern Empty = Alter []


-- ================================================================ --

toListRHS :: RHS t n h a -> [RHS t n h a]
toListRHS (AlterRHS rs) = rs
toListRHS r = [r]
{-# INLINE toListRHS #-}


-- ================================================================ --

{-| 

-}
{-# INLINEABLE hfmapRhsF #-}
hfmapRhsF
 :: (HFunctor h)
 => (f :~> g)
 -> (RhsF t n h f :~> RhsF t n h g) 

hfmapRhsF u = \case

 Pure a -> Pure a             -- must match GADT to change its type 
 rf `Apply` h -> u rf `Apply` (hfmap u h) 
 rf :<*> rx -> u rf :<*> u rx 

 Alter rs  -> Alter (u `map` rs)
 -- Opt  i x  -> i  <$> optional (go x)
 -- Many i x  -> i  <$> many     (go x)
 -- Some i x  -> i' <$> some     (go x) where i' = i . NonEmpty.fromList

 NonTerminal n r -> NonTerminal n (u r) -- TODO unshared 
 Terminal i t -> Terminal i t
 -- r@Terminals{} -> r 


-- {-| 

-- -}
-- {-# INLINEABLE hfoldMapRhsF #-}
-- hfoldMap
--  :: Monoid m
--  => (forall b. f b -> m)
--  -> h f a
--  -> m
-- hfoldMap u = \case 

--  Pure a -> Pure a             -- must match GADT to change its type 
--  rf `Apply` h -> u rf `Apply` (hfmap u h) 
--  rf :<*> rx -> u rf :<*> u rx 

--  Alter rs  -> Alter (u `map` rs)
--  -- Opt  i x  -> i  <$> optional (go x)
--  -- Many i x  -> i  <$> many     (go x)
--  -- Some i x  -> i' <$> some     (go x) where i' = i . NonEmpty.fromList

--  NonTerminal n r -> NonTerminal n (u r) -- TODO unshared 
--  Terminal i t -> Terminal i t
--  -- r@Terminals{} -> r 



{-| 

-}
{-# INLINEABLE htraverseRhsF #-}
htraverseRhsF
 :: (HTraversable h, Applicative m)
 => (f :~> (m :. g)) 
 -> (RhsF t n h f :~> (m :. RhsF t n h g)) 

htraverseRhsF u = \case
 Pure a -> pure $ Pure a             -- must match GADT to change its type 
 rf `Apply` h -> Apply <$> u rf <*> htraverse u h 
 rf :<*> rx -> (:<*>) <$> u rf <*> u rx 

 Alter rs  -> Alter <$> u `traverse` rs
 -- Opt  i x  -> i  <$> optional (go x)
 -- Many i x  -> i  <$> many     (go x)
 -- Some i x  -> i' <$> some     (go x) where i' = i . NonEmpty.fromList

 NonTerminal n r -> NonTerminal n <$> u r 
 Terminal i t -> pure $ Terminal i t
 -- r@Terminals{} -> r 


-- ================================================================ --


-- {-| 

-- -}
-- renameRHS :: (n1 -> m n2) -> RHS t n1 h a -> m (RHS t n2 h a) 
-- renameRHS u = 


-- {-| 

-- <http://stackoverflow.com/questions/13317242/what-are-paramorphisms what-are-paramorphisms>. 

-- -}
-- renameRHSByStableName :: RHS t n h a -> IO (RHS t (n, StableName a) h a) 
-- renameRHSByStableName = hapoparaM go -- TODO State, like hsearch 

--  where
--  go :: ((RHS t n h :*: RhsF t n h) :~> (IO :. RhsF t n h (RHS t n h :+: RhsF t n h)))
--  go (r0 :*: _r1) = case _r1 of 
--   NonTerminal n r -> (\s -> InR $ NonTerminal (n,s) r) <$> makeStableName r0 
--   _ -> return$ InR r0 


-- renameRHSByStableName :: RHS t n h a -> IO (RHS t (n, StableName a) h a) 
-- renameRHSByStableName = traverseNameRhsF go -- works on cyclic?  must be lazy?
--  where
--  go :: RHS t n h a -> IO (RHS t (n, StableName a) h a) 
--  go r0 = \case 
--   NonTerminal n r -> (\s -> NonTerminal (n,s) r) <$> makeStableName r0 
--   _ -> return r0



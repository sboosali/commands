{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, KindSignatures, StandaloneDeriving, RankNTypes, UndecidableInstances  #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PatternSynonyms, TypeOperators, ConstraintKinds, TypeFamilies, GeneralizedNewtypeDeriving, ScopedTypeVariables, RecordWildCards, PolyKinds    #-}
module Commands.RHS.Open where
import Commands.Extra

import Data.HFunctor.Recursion
import Data.HPrelude

--import Data.List.NonEmpty (NonEmpty) 
-- import qualified Data.List.NonEmpty as NonEmpty 
import           Control.Lens (Prism',prism)

import Data.Map (Map)
--import qualified Data.Map as Map
import Control.Monad.State
--import Control.Monad.Trans.State
import Data.Functor.Identity
import           Control.Applicative
--import Data.Void (Void)  
import           Data.Monoid
import           GHC.Exts            (IsList (..), IsString (..))
--import Data.Function
--import Control.Lens (Traversal')

import ReifiedBindings.HaskellBinding (HaskellBinding1(..))

--------------------------------------------------------------------------------

{- | a recursive 'RhsF'.

note on types: 

* @n@ (TODO) and @h@ are higher-kinded type constructors 

* @r@ is "fixed" to @(RHS t n h a)@ itself

similar to @'HFix' (RhsF t n h)@, but the fixpoint is specialized because: 

* @newtype@s provide better error messages than @type@s, and the user sees this type. 
* (more importantly) we can define domain-specific 'Applicative'/'Alternative' instances.

and:

@n@ provides sharing, @(n (RHS t n h))@. 

e.g. 

@
-- sharing is provided by reifying bindings
RHS t HaskellBinding1 h a

-- sharing is provided by pointer equality
RHS t StableName1 h a

-- unsafe, sharing is provided by a quote
RHS t (HConst 'Name') h a

-- sharing is tracked with an explicit reference
RHS t TVar1 h a
-- type TVar1 = HIdentity TVar

-- no sharing ('Identity1' in this "DSL" is analogous to inlining in Haskell)
RHS t Identity1 h a
-- type Identity1 = HIdentity Identity
-- pattern Identity1 a = HIdentity (Identity a)
@

-}
newtype RHS t (n :: (* -> *) -> (* -> *)) h a = RHS { unRHS :: RhsF
 (t)
 (n) -- e.g. n ~ HaskellBinding1, StableName1, HConst Name
 (h)
 (RHS t n h)
 (a)
 }

{-old


NO

{ n (RHS t n h) } doesn't work.
{ StableName } must take { a } too i.e. { StableName (RHS t n h a) }
sol- { n r a } not { n r }

HIdentity (RHS t n h) a
~
(RHS t n h) a

(HConst (Const String)) r a
~
Const String a
~
String

HVoid r a
~
Void


{ RhsF t n h (RHS t n h) a } means { r ~ (RHS t n h) } and { n r ~ n (RHS t n h) }

-- TODO should they know about all four type parameters, including each other.

newtype RHS t (n :: (* -> *) -> *) h a = RHS { unRHS :: RhsF
 (t)
 (n) -- e.g. n ~ HaskellBinding1, StableName1, HConst Name
 (h)
 (RHS t n h)
 (a)
 }

Identity1
HIdentity Identity a
Identity a
a

-}

{-old
(n (RHS t n h a)) -- e.g. n ~ HaskellBinding, StableName, Const Name
PROB HBase
-}

deriving instance (Functor'RHS t n h) => (Functor (RHS t n h)) -- NOTE uses UndecidableInstances

-- deriving instance () => Data (RHS t n h a)


{- | mostly lawful: 'fmap' and 'pure' behave lawfully. 

left-distributivity of '<*>' over '<|>' is intentionally violated. i.e. we want @(x \<|> y) \<*> z@ to be preserved, not to be distributed into @(x \<*> z) \<|> (y \<*> z)@. this helps: 

* when @(x \<|> y)@ is actually the infinite @(x \<|> y \<|> ...)@; interpreting the undistributed @(x \<|> y \<|> ...) \<*> z@ may terminate, while the distributed @(x \<*> z) \<|> (y \<*> z) \<|> ...@ will not.
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

{-old

type instance HBase (RHS t n h) a = RhsF t (n (RHS t n h a)) h a

   123  15 error           Number of parameters must match family declaration; expected 1
     In the type instance declaration for ‘HBase’ (intero)
-}

-- | 'hproject' = 'unRHS'
instance (HFunctor'RhsF n h) => HRecursive (RHS t n h) where hproject = unRHS

-- | 'hinject' = 'RHS'
instance (HFunctor'RhsF n h) => HCoRecursive (RHS t n h) where hinject = RHS

-- | @ConstraintKinds@
type Functor'RHS t n h =
 ( Functor (n (RHS t n h))
 , Functor (h (RHS t n h))
 )

-- | @pattern EmptyRHS = 'RHS' ('Alter' [])@
pattern EmptyRHS = RHS (Alter []) -- TODO AlterF 

-- | @pattern AlterRHS rs = 'RHS' ('Alter' rs)@
pattern AlterRHS rs = RHS (Alter rs) 

-- -- | @pattern ManyRHS r = 'RHS' ('Many' r)@
-- pattern ManyRHS r = RHS (Many r) 

-- -- | @pattern SomeRHS r = 'RHS' ('Some' r)@
-- pattern SomeRHS r = RHS (Some r) 

{-

= \case 
 Pure a -> pure$ Pure a             -- must match GADT to change its type 
 rf `Apply` h -> pure$ rf `Apply` h 
 rf :<*> rx -> pure$ rf :<*> rx 

 Alter rs  -> pure$ Alter rs 
 -- Opt  i x  -> pure$ Opt  i x  
 -- Many i x  -> pure$ Many i x
 -- Some i x  -> pure$ Some i x  

 NonTerminal n r -> pure$ NonTerminal n r 
 Terminal i t -> pure$ Terminal i t
 -- r@Terminals{} -> pure$ r 

-}



--------------------------------------------------------------------------------

{-| a non-recursive and lower-order 'RhsF'. 

note on types: 

* @Void@          : knocks out the 'NonTerminal' constructor, since @0 * a = 0@. 
* @(HConst f)@    : lifts the given lower-order functor.  
* @(Const n)@     : direct reference becomes indirect reference.  
* @()@            : trivializes the injections (e.g. 'Pure'), since @1^a = 1@.   

-}
newtype RHS0 t n f = RHS0 { unRHS0 :: RhsF
 (t)
 (HVoid)
 (HConst f)
 (Const n)
 ()
 }

{-| un-associated -}
-- type FlatRHS0 t f = RHS0 t (HVoid) f

{-


no {n} means no sharing

RhsF (n : (* -> *) -> *) (r : * -> *) (a : *)
because
n r
r a

r "is" String
n "is" also String?


When is {r} not RHS?
Other "self"


newtype RHS0 t n f = RHS0 { unRHS0 :: RhsF
 (t)
 (HConst (Const n)) 
 (HConst f)
 (Const n)
 ()
 }


{n} becomes void
{r} becomes {n}

OR

{n} stays {n}
{r} becomes void

NO?
{n} declares a left-hand-side.
if {Grammar0 = Map n RHS0}, then each item can declare the left-hand-side.

YES?
{n} provides sharing, no more and no less
can link {n} to the {Map n r}

Production = LHS + RHS = NonTerminal + <rest>
Production n~Void = RHS only

-}


{-| 

-}
-- type ReifiedRHS t n f = Map n (RHS0 t n f)


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

data Void1 (f :: * -> *) 

data HVoid (f :: * -> *) (a :: *)

-- generalizeRHS :: RHS t n HVoid r a -> RHS t n h r a
-- generalizeRHS = \case

--TODO {h} extends, {n} shares

{- | the "1" and "0" mean it takes a unary type and a nullary type.

'Const' would be "Const0".

@ ~ 'HConst' 'Identity'@

-}
newtype Const10 b (r :: * -> *) a = Const10 b

--------------------------------------------------------------------------------

{- | a 
<https://en.wikipedia.org/wiki/Extended_Backus-Naur_Form EBNF>
grammar.

EBNF features (using Wikipedia's jargon):

* "terminals" come from 'Terminal'. 
* "non-terminals" come from 'NonTerminal' 
* "alternation" comes from the 'Alternative' instance, via 'Alter'.
* "sequencing" comes from 'Applicative' instance, via  'Apply' / ':<*>'.
* "epislon"s come from 'Pure' (TODO or @'Alter' []@?).
* "optionality" comes from 'Opt'.
* "repetition" come from 'Many' / 'Some'.
* ("grouping" just comes from Haskell's precedence/parentheses.)
* (*no* "exceptions")

(TODO and thus, the name "RHS" is a bit of a lie, as this one case can hold a "LHS" too).
(rename to Grammar or G)

In fact it's a <http://bnfc.digitalgrammars.com/ Labeled BNF>.

LBNF features:

* a constructor on the left of '<$>' labels the grammatical sequence (e.g. @f <$> a <*> b@ labels the sequence of @a@ and @b@ with @f@).
* the type of '<*>' is heterogeneous, which lets us capture right-hand-sides of different types

Furthermore:

* extensibility is provided by the @h@ type: 

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

when partially applied, an @RhsF t n h@ has the same kind-arity as its @h@: 

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

Induces a family of datatypes, which are different specializations:

* 'RHS'
* 'RHS0'

-}
data RhsF
 :: *
 -> ((* -> *) -> (* -> *))
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

 NonTerminal :: n r a             -> RhsF t n h r a -- TODO one field?  :: n -> RhsF t n h r a  TODO move into RHS? 
 Terminal    :: (t -> a)   -> !t  -> RhsF t n h r a
 -- Terminals   :: (t -> a)                         -> RhsF t n h r a  -- placeholder for the set of terminals in the grammar TODO should be part of @h@

{-OLD

h r a
n r a
r a
a


deriving instance (Traversable'RhsF n h r) => (Traversable (RhsF t n h r))

   458   1 error           Can't make a derived instance of ‘Traversable (RhsF t n h r)’:
       Constructor ‘Apply’ must not contain function types
     In the stand-alone deriving instance for
       ‘(Traversable'RhsF n h r) => (Traversable (RhsF t n h r))’ (intero)


-}

-- | 
deriving instance (Functor'RhsF n h r) => (Functor (RhsF t n h r))

-- | 

-- | 'hfmapRhsF'
instance (HFunctor'RhsF n h) => HFunctor (RhsF t n h) where
  hfmap = hfmapRhsF
  {-# INLINEABLE hfmap #-}

-- instance (HFoldable h) => HFoldable (RhsF t n h) where
--  hfoldMap = hfoldMapRhsF 
--  {-# INLINEABLE hfold #-}

-- | 'htraverseRhsF'
instance (HTraversable'RhsF n h) => HTraversable (RhsF t n h) where
  htraverse = htraverseRhsF 
  {-# INLINEABLE htraverse #-}

{- | both token and result must be an (instance of) 'IsString'.

(see <http://chrisdone.com/posts/haskell-constraint-trick the constraint trick>)

@t@ can default to String.

'fromStringRhsF'

-}
instance (IsString t, Show t, a ~ t) => IsString (RhsF t n h r a) where --TODO remove Show constraint or show it's needed for defaulting
 fromString = fromStringRhsF
 {-# INLINEABLE fromString #-}

-- -- | @([r1,r2,r3] :: RHS t n h a)@ is @('mconcat' [r1,r2,r3])@ is @('asum' [r1,r2,r3])@ is @(r1 '<|>' r2 '<|>' r3)@
-- instance IsList (RhsF t n h r a) where
--  type Item (RhsF t n h r a) = (RHS t n h a)
--  fromList = Alter             -- the constructor (rather than a method like "asum") avoids the (Functor f) constraint
--  toList = toListRHS

-- | @ConstraintKinds@
type Functor'RhsF n h r =
 ( Functor (n r)
 , Functor (h r)
 , Functor r
 )

-- | @ConstraintKinds@
type Traversable'RhsF n h r =
 ( Traversable (n r)
 , Traversable (h r)
 , Traversable r
 )

-- | @ConstraintKinds@
type HFunctor'RhsF n h =
 ( HFunctor n
 , HFunctor h
 )

-- | @ConstraintKinds@
type HTraversable'RhsF n h =
 ( HTraversable n
 , HTraversable h
 )

-- | @pattern Empty = 'Alter' []@
pattern Empty = Alter []

--------------------------------------------------------------------------------

fromStringRhsF :: (IsString t, Show t, a ~ t) => String -> (RhsF t n h r a)
fromStringRhsF s = Terminal id t where t = fromString s
{-# INLINEABLE fromStringRhsF #-}

toListRHS :: RHS t n h a -> [RHS t n h a]
toListRHS (AlterRHS rs) = rs
toListRHS r = [r]
{-# INLINE toListRHS #-}

{-| 

-}
{-# INLINEABLE hfmapRhsF #-}
hfmapRhsF
 :: (HFunctor'RhsF n h)
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

 NonTerminal n -> NonTerminal (hfmap u n) -- TODO unshared 
 Terminal i t -> Terminal i t

fmap1 :: (f :~> g) -> (h f -> h g)
fmap1 _u = undefined --TODO

-- class Functor1 h where
--  fmap1 :: (f :~> g) -> (h f -> h g)

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
 :: (HTraversable'RhsF n h, Applicative m)
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

 NonTerminal n -> NonTerminal <$> htraverse u n
 Terminal i t -> pure $ Terminal i t
 -- r@Terminals{} -> r 

traverse1 :: (f :~> (m :. g)) -> (h f -> m (h g))
traverse1 _u = undefined --TODO

{-OLD

{-# INLINEABLE traverseRhsF #-}
traverseRhsF
 :: (Traversable'RhsF n h r, Applicative m)
 => (a -> m b)
 -> (RhsF t n h r a -> m (RhsF t n h r b)) 

traverseRhsF f = \case
 Pure a -> Pure <$> f a
-- rf `Apply` hrx -> Apply <$> traverse (traverse f) rf <*> traverse (traverse f) hrx 
-- rf :<*> rx -> (:<*>) <$> traverse f rf <*> f rx 

 Alter rs  -> Alter <$> traverse f `traverse` rs
 -- Opt  i x  -> i  <$> optional (go x)
 -- Many i x  -> i  <$> many     (go x)
 -- Some i x  -> i' <$> some     (go x) where i' = i . NonEmpty.fromList

 NonTerminal n -> NonTerminal <$> traverse f n
 Terminal i t -> Terminal <$> traverse f i <*> pure t
 -- r@Terminals{} -> r 

   637  31 error           Could not deduce (Traversable ((->) t))
       arising from a use of ‘traverse’
     from the context (Traversable'RhsF n h r, Applicative m)
       bound by the type signature for
                  traverseRhsF :: (Traversable'RhsF n h r, Applicative m) =>
                                  (a -> m b) -> RhsF t n h r a -> m (RhsF t n h r b)

-}

--------------------------------------------------------------------------------

_NonTerminalF :: Prism' (RhsF t n h r a) (n r a)
_NonTerminalF = prism NonTerminal $ \case
 NonTerminal n -> Right n
 r             -> Left r

_NonTerminal :: Prism' (RHS t n h a) (n (RHS t n h) a)
_NonTerminal = prism _inject _project
 where
 _inject = NonTerminal >>> RHS
 _project r = case unRHS r of
   NonTerminal n -> Right n
   _             -> Left r

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


{-| 

-}
{-# INLINEABLE ntraverseRHS #-}
ntraverseRHS
 :: (HTraversable h, Applicative m)
 => (n1 r :~> (m :. n2 r)) 
 -> (RHS t n1 h  :~> (m :.     RHS t n2 h)) 

ntraverseRHS u = \case
  _ -> pure undefined

{-old
 => (n1 (RHS t n1 h) :~> (m :. n2 (RHS t n2 h))) 
 => (n1 (RHS t n h) :~> (m :. n2 (RHS t n h))) 
 => (n1 r :~> (m :. n2 r)) 

r is RHS..


-}

-- {-| 

-- -}
-- {-# INLINEABLE ntraverseRhsF #-}
-- ntraverseRhsF
--  :: (HTraversable h, Applicative m)
--  => (n1 r :~> (m :. n2 r)) 
--  -> (RhsF t n1 h r :~> (m :. RhsF t n2 h r)) 

-- ntraverseRhsF u = \case
--   _ -> pure undefined

--old => (forall x. RhsF t n1 h r x -> n1 r x -> (m :. n2 r) x) 

-- nmapRhsF
--  :: (n1 -> n2)
--  -> (RHS t n1 h :~> RHS t n2 h)
-- nmapRhsF = underIdentity ntraverseRhsF

-- underIdentity :: (f a -> f b) -> (a -> b) -> (f a -> f b) -- @n@ isn't final type parameter 
-- underIdentity f g = getIdentity . f (Identity . g) -- TODO see ala https://hackage.haskell.org/package/lens-4.13/docs/Control-Lens-Iso.html


-- ntraverseRhsF
--  :: (n1 -> m n2)
--  -> (RHS t n1 h :~> (m :. RHS t n2 h))
-- ntraverseRhsF u = 
--  where
--  go = \case 
--   NonTerminal n r -> NonTerminal <$> u n <*> pure r
--   r -> pure r  -- unsafeCoerce? for convenience and efficiency. is the memory representation the same for all @n :: *@? 

-- {-| 

-- given two outputs, @(let r1 = NonTerminal (n1, s1) _)@ and @(let r2 = NonTerminal (n2, s2) _)@, 
-- then @s1==s2@ implies @n1==n2@, but not vice versa.    

-- -}
-- observeSharingRHS :: forall t n h a. (HTraversable h) => RHS t n h a -> IO (RHS t (n, Exists StableName) h a) 
-- observeSharingRHS = traverseRHS go -- TODO works on cyclic?  must be lazy?  use Pipe, not Traversable? might as well run State and IO at the same time, since consuming the structure-with-interleaving would be impure anyway 
--  where
--  go :: forall r x. RhsF t n h r x -> IO (RhsF t (n, Exists StableName) h r x) 
--  go = \case 
--   r0@(NonTerminal n r) -> (\s -> NonTerminal (n, Exists s) r) <$> makeStableName r0 

--   -- not {r -> pure r}, must match a GADT to change its type 

--   Terminal i t -> pure$ Terminal i t

--   Pure a -> pure$ Pure a           
--   rf `Apply` h -> pure$ rf `Apply` h 
--   rf :<*> rx -> pure$ rf :<*> rx 

--   Alter rs  -> pure$ Alter rs 
--  -- Opt  i x  -> pure$ Opt  i x  
--  -- Many i x  -> pure$ Many i x
--  -- Some i x  -> pure$ Some i x  



-- {-| 

-- -}
-- {-# INLINEABLE traverseRHS #-}
-- traverseRHS
--  :: (HTraversable h, Applicative m)
--  => (forall r x. RhsF t n h r x -> m (RhsF t' n' h r x))
--  -> (            RHS  t n h   a -> m (RHS  t' n' h   a)) 
-- -- traverseRHS u = cataM u 
-- traverseRHS = undefined 

-- TODO for RHS, a ReaderT (Algebra h), explicit dictionary 

--------------------------------------------------------------------------------

{-| a 'NonTerminal' TODO

-}
type Grammar = RHS

{-|

-}
-- type Production t n h a = (n a, RHS t n h a)

{-|

-}
data Grammar0 t n f = Grammar0 
 { rootGrammar0  :: Maybe n --TODO, or Should homogenizeRHS The partial Instead?
 , rulesGrammar0 :: Map n (RHS0 t n f)
 }
--type Grammar0 t n f = (n, Map n (RHS0 t n f))

{-|

-}
type HomogenizingRHS t n f = StateT (Map n (RHS0 t n f)) Identity ()

{-|

the output's 'NonEmpty.head' shares the same name @n@ with the input.
the 'NonEmpty.tail' are its descendent productions.

NOTE all recursion must be via 'NonTerminal'. i.e.:

@
-- explicit recursion is good
list_digit = 'list_digit <=> (:) <$> digit <*> list_digit

-- implicit recursion is bad
list p = (:) <$> p <*> list p
@

Otherwise, this may not terminate.

-}

-- homogenizeRHS :: Grammar t n (HConst f) x -> Grammar0 t n f
-- homogenizeRHS g = Grammar0{..}
--  where
--  rootGrammar0  = g & fromNonTerminal & fmap fst
--  rulesGrammar0 = g & homogenizeRHS & (execState&flip) Map.empty 

-- {-| Internal. 

-- -}
-- _homogenizeRHS :: Grammar t n (HConst f) x -> HomogenizingRHS t n f
-- _homogenizeRHS = undefined --TODO Recursion scheme?

-- -- fromNonTerminal :: RHS t n h a -> Production t n h a
-- -- fromNonTerminal r = case r of
-- --   NonTerminal n -> Just (n, r)
-- --   _ -> Nothing

-- renameDNSEarleyFunc
--  :: forall m n1 n2 t f1 f2 a. ((f1 ~ DNSEarleyFunc n1 t), (f2 ~ DNSEarleyFunc n2 t))
--  => (Applicative m)
--  => (forall x. RHS n1 t f1 x -> n1 t f1 x -> m (    n2 t f2 x))
--  -> (                       RHS n1 t f1 a -> m (RHS n2 t f2 a))
-- renameDNSEarleyFunc u = \case
--  k@(NonTerminal x r)  ->  NonTerminal <$> u k x <*> go r -- like traverse, except this case
--  Terminal i r       ->  pure$ Terminal i r
--  Terminals i        ->  pure$ Terminals i 
--  Opt  i r           ->  Opt  i <$> go r
--  Many i r           ->  Many i <$> go r
--  Some i r           ->  Some i <$> go r
--  Pure a             ->  pure$ Pure a
--  r `Apply` x        ->  Apply <$> go r <*> (case x of
--   TreeRHS pRHS rRHS ->  TreeRHS <$> go pRHS <*> go rRHS
--   LeafRHS p s       ->  pure$ LeafRHS p s)
--  r :<*> r'          ->  (:<*>) <$> go r <*> go r'
--  Alter rs           ->  Alter <$> go `traverse` rs
--  where
--  go :: forall x. RHS n1 t f1 x -> m (RHS n2 t f2 x)
--  go = renameDNSEarleyFunc u


-- renameDNSEarleyRHSIO
--  :: ((f1 ~ DNSEarleyFunc n1 t), (f2 ~ DNSEarleyFunc n2 t))
--  => (forall x. RHS n1 t f1 x -> n1 t f1 x -> IO (    n2 t f2 x))
--  -> IO                   (RHS n1 t f1 a -> IO (RHS n2 t f2 a))
-- renameDNSEarleyRHSIO u = do
--  c <- HRefCache.newCache
--  return$ renameDNSEarleyFunc$ \r1 n -> do
--   k <- HRefCache.forceStableName r1
--   readIORef c >>= (HRefCache.lookupRef k >>> traverse readIORef) >>= \case
--    Just y  -> return y          -- cache hit
--    Nothing -> do                -- cache miss
--     y <- u r1 n
--     v <- newIORef y
--     _ <- atomicModifyIORef' c ((,()) . HRefCache.insertRef k v)
--     return y


-- {-|

-- --TODO traversal for Terminals. e.g. to Modify Each terminal with alias; e.g. "sell" -> "select" for Postprocessing The edit grammar To make it recognizable on a Non-trainable Speech engine.

-- -}
-- terminalsRHS :: Traversal' (RHS t n h a) t
-- terminalsRHS u = \case
  
--  Terminal    i t -> Terminal i <$> u t
--  NonTerminal r  -> NonTerminal <$> go r

--  Alter rs  -> Alter <$> go `traverse` rs
--  -- Opt  i x  -> i  <$> optional (go x)
--  -- Many i x  -> i  <$> many     (go x)
--  -- Some i x  -> i' <$> some     (go x) where i' = i . NonEmpty.fromList

--  Pure a -> pure $ Pure a
--  rf `Apply` h -> Apply <$> go rf <*> htraverse go h 
--  rf :<*> rx -> (:<*>) <$> go rf <*> go rx 

--  where
--  go = terminalsRHS u

--TODO recursive Traversal (RHS ..) t
--TODO non-recursive Traversal (RhsF ..) t

--------------------------------------------------------------------------------

newtype AcyclicRHS t (n :: (* -> *) -> (* -> *)) h a = AcyclicRHS { unAcyclicRHS ::
 RHS t n h a
} 
                                             
rhsIsAcyclic :: RHS t HaskellBinding1 h :~> AcyclicRHS t HaskellBinding1 h
rhsIsAcyclic = undefined


--------------------------------------------------------------------------------

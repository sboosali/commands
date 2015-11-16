{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, KindSignatures, StandaloneDeriving, RankNTypes, UndecidableInstances  #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PatternSynonyms, TypeOperators, ConstraintKinds #-}
module Commands.RHS.Open where
import Data.HFix

-- import Data.List.NonEmpty (NonEmpty) 

import           Control.Applicative
import Data.Void (Void)  


{- | a recursive 'RhsF'. 

types: 

* @n@ and @f@ are higher-kinded type constructors (they know about all four type parameters, including each other). 

* @r@ is "fixed" to @(RHS t n h a)@ itself

isomorphic to @HFix (RhsF t n h)@, but the fixpoint is specialized because: 

* @newtype@s provide better error messages than @type@s, and the user sees this type. 
* more importantly, we can define domain-specific 'Applicative'/'Alternative' instances. 

-}
newtype RHS t n h a = RHS { unRHS :: RhsF
 (t)
 (n)
 (h) 
 (RHS t n h)
 (a)
 }

deriving instance (Functor (h (RHS n t h))) => (Functor (RHS n t h)) -- NOTE uses UndecidableInstances
-- deriving instance () => Data (RHS n t h a)

{- | mostly lawful. 'fmap' and 'pure' behave lawfully. 

left-distributivity of '<*>' over '<|>' is intentionally violated. that is, we want @(x \<|> y) \<*> z@ to be preserved, not to be distributed into @(x \<*> z) \<|> (y \<*> z)@. this helps: 

* when @(x \<|> y)@ is actually the infinite @(x \<|> y \<|> ...)@, interpreting the undistributed @(x \<|> y) \<*> z@ might terminate while the @(x \<|> y \<|> ...) \<*> z@ may terminate while the distributed @(x \<*> z) \<|> (y \<*> z) \<|> ...@ will not.
* when the interpretation (e.g. a chart parser) can increase performance by sharing such "inner alternation". 

'<*>' is left-associated. 

-}
instance (Functor (h (RHS n t h))) => Applicative (RHS n t h) where
 {-# INLINEABLE pure #-} 
 pure = RHS . Pure

 {-# INLINEABLE (<*>) #-} 

 RHS (Pure xa) <*> RHS rx                = RHS$ fmap xa rx        -- Functor
 -- Pure {id} <*> x            = fmap {id} x     = x              -- Identity
 -- Pure f <*> Pure x          = fmap f (Pure x) = Pure (f x)     -- Homomorphism
 RHS rxa     <*> RHS (Pure x)            = RHS$ fmap ($ x) rxa    -- Interchange

 RHS Empty    <*> _                  = RHS Empty                            -- left-Annihilation (?)
 _            <*> RHS Empty          = RHS Empty                            -- right-Annihilation

 rxa <*> rx = RHS$ case rx of

  RHS (ryx `Apply` hy) -> ((.) <$> rxa <*> ryx) `Apply` hy -- Composition
  RHS (ryx :<*>    ry) -> ((.) <$> rxa <*> ryx) :<*>    ry -- Composition
  RHS (Alter _rxs)     -> rxa :<*> rx                      -- NO left-Distributivity

 --  r@(Opt  _ysa _ty)     = RHS$ txa :<*> RHS r    -- NOTE intentionally doesn't distribute
 --  r@(Many _ysa _ty)     = RHS$ txa :<*> RHS r    -- NOTE intentionally doesn't distribute
 --  r@(Some _ysa _ty)     = RHS$ txa :<*> RHS r    -- NOTE intentionally doesn't distribute

  RHS (NonTerminal{}) -> rxa :<*> rx -- NOTE preserving sharing is critical for the observers sharing interface 
  RHS (Terminal   {}) -> rxa :<*> rx 
 -- txa     <*> r@(Terminals _i)      = RHS$ txa :<*> r -- NOTE greatly simplifies "self-referential" grammars (self-recursive grammars are already simple)

-- | @pattern EmptyRHS = RHS ('Alter' [])@
pattern EmptyRHS = RHS (Alter []) -- TODO AlterF 



-- ================================================================ --

{-| a non-recursive and lower-order 'RhsF'. 


-}
newtype RHS_ t n f = RHS_ { unRHS_ :: RhsF -- TODO naming: RHS0?  
 (t)
 (Void)                         -- knocks out the NonTerminal constructor, since @0 * a = 0@. 
 (HConst f)                     -- lifts the given lower-order functor.  
 (Const n)                      -- direct recursion becomes indirect recursion.  
 ()                             -- trivializes the injections, since @1^a = a@.   
 }


-- reifyRHS :: RHS t n (HConst f) a -> [(n, RHS_ t n f)]  



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

extensibility comes from the @h@. 
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

deriving instance (Functor'RhsF h r) => (Functor (RhsF n t h r))

-- | @ConstraintKinds@
type Functor'RhsF h r =
 ( Functor r
 , Functor (h r)
 )

-- | @pattern Empty = 'Alter' []@
pattern Empty = Alter []

instance (HFunctor h) => HFunctor (RhsF t n h) where
 hfmap = hfmapRhsF
 {-# INLINEABLE hfmap #-}

{-| 

-}
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

{-# INLINEABLE hfmapRhsF #-}



-- ================================================================ --

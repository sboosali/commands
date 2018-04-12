{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms      #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, StandaloneDeriving       #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, ConstraintKinds               #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fno-warn-missing-signatures  #-}
module Commands.RHS.Types where
import Commands.Extra (Exists) 

import           Control.Lens hiding (Empty) -- TODO 
import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty  as NonEmpty

import           Control.Applicative
import           Data.Monoid
import           GHC.Exts            (IsList (..), IsString (..))
-- import Text.Printf

import Prelude

newtype ConstNonTerminal n t (f :: (* -> *)) a = ConstNonTerminal n

{-| a grammatical right hand side.  


-}
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
pattern Empty :: forall (n :: * -> (* -> *) -> * -> *) t (f :: * -> *) a. RHS n t f a
pattern Empty = Alter []

-- | @ConstraintKinds@
type Functor'RHS n t f = (Functor (n t f), Functor f)

-- type RHSFunctorC n t f = (Functor f, Functor (n t f)) ConstraintKinds

deriving instance (Functor (n t f)) => (Functor (RHS n t f)) -- TODO expects constraint:
-- deriving instance () => Data (RHS n t f a)

-- | lawful (coincides with 'Alternative' instance)
instance (Functor f, Functor (n t f)) => Monoid (RHS n t f a) where
 mempty = Empty
 mappend = (<|>)

{- | mostly lawful. 'fmap' and 'pure' behave lawfully. 

left-distributivity of '<*>' over '<|>' is intentionally violated. that is, we want @(x \<|> y) \<*> z@ to be preserved, not to be distributed into @(x \<*> z) \<|> (y \<*> z)@. this helps: 

* when @(x \<|> y)@ is actually the infinite @(x \<|> y \<|> ...)@, interpreting the undistributed @(x \<|> y) \<*> z@ might terminate while the @(x \<|> y \<|> ...) \<*> z@ may terminate while the distributed @(x \<*> z) \<|> (y \<*> z) \<|> ...@ will not.
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

 txa     <*> (tyx `Apply` fy)  = ((.) <$> txa <*> tyx) `Apply` fy -- Composition
 txa     <*> (tyx :<*>    ty)  = ((.) <$> txa <*> tyx) :<*>    ty -- Composition

 txa     <*> r@(Alter _txs)        = txa :<*> r               -- NO left-Distributivity

 txa     <*> r@(Opt  _ysa _ty)     = txa :<*> r -- NOTE doesn't distribute, intentionally 
 txa     <*> r@(Many _ysa _ty)     = txa :<*> r -- NOTE doesn't distribute, intentionally 
 txa     <*> r@(Some _ysa _ty)     = txa :<*> r -- NOTE doesn't distribute, intentionally 

 txa     <*> r@(Terminal    _i _t) = txa :<*> r 
 txa     <*> r@(NonTerminal _l _r) = txa :<*> r -- NOTE preserving sharing is critical for the observers sharing interface 
 txa     <*> r@(Terminals _i)      = txa :<*> r -- NOTE greatly simplifies "self-referential" grammars (self-recursive grammars are already simple)


-- | lawful.  
instance (Functor f, Functor (n t f)) => Alternative (RHS n t f) where
 empty = Empty

 Empty <|> y     = y                                  -- Left-Identity
 x     <|> Empty = x                                  -- Right-Identity
 x     <|> y     = Alter (toRHSList x <> toRHSList y) -- Associativity
 {-# INLINE (<|>) #-}

 many = Many id
 {-# INLINE many #-}

 some = fmap NonEmpty.toList . Some id
 {-# INLINE some #-}


{- | both token and result must be an (instance of) 'IsString'.

(see <http://chrisdone.com/posts/haskell-constraint-trick the constraint trick>)

@t@ can default to String.

-}
instance (IsString t, Show t, a ~ t) => IsString (RHS n t f a) where --TODO remove Show constraint or show it's needed for defaulting
 fromString s = Terminal id t where t = fromString s
 {-# INLINEABLE fromString #-}
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

{-| a "lowered" (existentially-quantified) right hand side.  

-}
type RHS0 n t f = Exists (RHS n t f)

-- | e.g. @('RHS' (ConstName n) t f a)@
data ConstName n t (f :: * -> *) a = ConstName { _unConstName :: !n } deriving (Functor)
-- KindSignatures because: f being phantom, it's kind is inferred to be nullary (I think)
-- TODO is PolyKinds better? (f :: k)
deriving instance Show n => Show (ConstName n t f a)

data SomeRHS n t f = SomeRHS { _unSomeRHS :: forall x. RHS n t f x }


-- ================================================================ --

toRHSList :: RHS n t f a -> [RHS n t f a]
toRHSList (Alter xs) = xs
toRHSList x = [x]
{-# INLINE toRHSList #-}


-- ================================================================ --
-- lenses

_RHSName :: Traversal' (RHS n t f a) (n t f a)
_RHSName = _NonTerminal._1

_NonTerminal :: Prism' (RHS n t f a) (n t f a, RHS n t f a)
_NonTerminal = prism (uncurry NonTerminal) $ \case
 NonTerminal l r -> Right (l, r)
 r -> Left r

-- makePrisms ''RHS
makeLenses ''ConstName
makeLenses ''SomeRHS

--TODO refactor -? to .? conflicts with lens?


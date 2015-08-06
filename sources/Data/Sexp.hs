{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable, DeriveFunctor        #-}
{-# LANGUAGE LambdaCase, OverloadedLists, OverloadedStrings, TypeFamilies #-}
module Data.Sexp where

import Control.Lens
import Data.Semigroup

import Data.Foldable  (Foldable (..))
import GHC.Exts       (IsList (..), IsString (..))


{- | a heterogenous list.
a <http://en.wikipedia.org/wiki/Common_Lisp#The_function_namespace Lisp-2> S-expression, where:

* @f@ is the function namespace
* @a@ is the atom namespace

you could @type Lisp1 a = Sexp a a@, but:

* @f@ is ignored by 'Monad'ic methods like 'joinSexp'
* you might want to make sure to eval the @f@ to some normal form,
like when matching against it in 'evalSexp'
* @plate@ doesn't reach the @f@, even when @f ~ a@,
as the 'Plated' instance is manual, not automatic via @Data@.

'List' is just a specialized @'Sexp' ()@, but easier to work with than:

* @Sexp (Maybe f) [Sexp f a]@ (where Nothing would represent 'List')
* forcing each concrete @f@ to hold a unit (which would represent 'List')


-}
data Sexp f a
 = Atom a
 | List   [Sexp f a]
 | Sexp f [Sexp f a]
 deriving (Show,Eq,Ord, Functor)

-- | default from the 'Monad' subclass.
instance Applicative (Sexp f) where
 pure = return
 (<*>) f x = f >>= (\g -> x >>= (return.g))

{- |

@
 'return' = 'returnSexp'
 '(>>=)' = 'bindSexp'
@

proofs of laws:


* TODO : @join . return = id@

@
joinSexp (returnSexp m)
joinSexp (Atom m)
m
@


* TODO : @join . fmap return = id@

@
joinSexp (fmap returnSexp m)
TODO

m
@


* associativity: @join . join = join . fmap join@

@
TODO
@



laws, verified as @QuickCheck@ properties:

TODO


-}
instance Monad (Sexp f) where
 return = returnSexp
 (>>=) = bindSexp

instance Semigroup (Sexp f a) where (<>)   = appendSexp
instance Monoid    (Sexp f a) where mempty = nilSexp; mappend = (<>)

instance Plated (Sexp f a) where
 plate f = \case
  Atom a    -> Atom   <$> pure a
  List   ps -> List   <$> traverse f ps
  Sexp g ps -> Sexp g <$> traverse f ps

-- |
instance () => IsList (Sexp f a) where
 type Item (Sexp f a) = (Sexp f a)
 fromList = List
 toList = toSexpList

-- |
instance (IsString a) => IsString (Sexp f a) where fromString = Atom . fromString


returnSexp :: a -> Sexp f a
returnSexp = Atom
{-# INLINE returnSexp #-}

bindSexp :: Sexp f a -> (a -> Sexp f b) -> Sexp f b
bindSexp s f = (joinSexp . fmap f) s
{-# INLINE bindSexp #-}

joinSexp :: Sexp f (Sexp f a) -> Sexp f a
joinSexp = \case
 Atom     s -> s
 List   sss -> List   (joinSexp <$> sss)
 Sexp f sss -> Sexp f (joinSexp <$> sss)
{-# INLINEABLE joinSexp #-} -- to hit the Atom I hope.

-- | refines any Sexp to a list, which can be given to the List variant.
--
toSexpList :: Sexp f a -> [Sexp f a]
toSexpList = \case
 List ss -> ss
 s       -> [s]
{-# INLINE toSexpList #-}

-- |
appendSexp :: Sexp f a -> Sexp f a -> Sexp f a
appendSexp x y = List (toSexpList x <> toSexpList y)
{-# INLINE appendSexp #-}

nilSexp :: Sexp f a
nilSexp = List []
{-# INLINE nilSexp #-}

-- | strictly evaluate a sexp ("all the way") to an atom.
--
-- e.g. when specializing @f ~ ((->) e)@, then
--
-- @evalSexp :: ([a] -> e -> a) -> ([a] -> g -> e -> a) -> Sexp g a -> (e -> a)@
--
-- and evaluation just takes an environment, passing it down to the two evaluators.
--
evalSexp :: (Monad m) => ([a] -> m a) -> ([a] -> g -> m a) -> Sexp g a -> m a
evalSexp list apply = \case
 Atom a    -> return a
 List   ss -> list           =<< mapM (evalSexp list apply) ss
 Sexp g ss -> (flip apply) g =<< mapM (evalSexp list apply) ss

-- |
--
-- when a Sexp\'s atoms are 'Monoid'al ("list-like"),
-- after evaluating some expressions into atoms,
-- we can "splat" them back together.
--
-- @splatList@ takes: a monadic evaluator @eval@
-- and a list of s-expressions @ss@ to evaluate in sequence.
splatList :: (Applicative m, Monoid xs) => (Sexp g xs -> m xs) -> [Sexp g xs] -> m xs
splatList eval ss = fold <$> traverse eval ss

-- |
--
-- e.g. @evalSplatSexp :: ([x] -> g -> Maybe [x]) -> (Sexp g [x] -> Maybe [x])@
--
evalSplatSexp :: (Applicative m, Monad m, Monoid xs) => (xs -> g -> m xs) -> (Sexp g xs -> m xs)
evalSplatSexp apply = evalSexp (return.fold) (apply.fold)

exampleSexp :: Sexp f String
exampleSexp = ["f", "x", ["g", "y"], "z"]

exampleSexpBlock :: Sexp f String
exampleSexpBlock = do
  x <- exampleSexp
  listSexp [x,x]

listSexp :: [a] -> Sexp f a
listSexp = List . map Atom

data SexpF a r
 = AtomF a
 | ListF [r]
 deriving (Show,Eq,Ord, Functor)

-- type Sexp' a = Fix (SexpF a)


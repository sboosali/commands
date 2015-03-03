{-# LANGUAGE DeriveFunctor, GADTs, TypeFamilies #-}
module Control.Alternative.Free.Tree where
import Control.Applicative
-- import           Data.Foldable       (Foldable (..))
-- import qualified Data.Foldable       as Foldable
import Data.Monoid

-- if it's not Applicative/Alternative, it won't work with Permutation, and you should just use the first easier-but-weaker fixed-length vinyl design

-- |
data Alt f a where
 Pure   ::                     a -> Alt f a
 Many   ::             [Alt f a] -> Alt f a
 App    :: Alt f (x -> a) -> f x -> Alt f a
 (:<*>) :: Alt f (x -> a) -> Alt f x -> Alt f a -- just for Many.

instance Functor f => Functor     (Alt f) where
 fmap ab (Pure a) = Pure (ab a)
 fmap ab (Many fas) = Many (fmap (fmap ab) fas)
 fmap ab (fxa `App` hx) = fmap (ab .) fxa `App` hx
 fmap ab (fxa :<*> fx) = fmap (ab .) fxa :<*> fx

-- lawless
instance Functor f => Applicative (Alt f) where
 pure  = Pure
 Pure xa <*> fx            = fmap xa fx       -- Functor
 fxa     <*> Pure x        = fmap ($ x) fxa   -- Interchange
 _       <*> Many []       = Many []          -- Annihilation
 fxa     <*> Many fxs      = fxa :<*> Many fxs             -- don't distribute, as @fxs@ may be unbounded.
 -- Thus it's not linearly left-associated: treelike, not listlike.
 fxa     <*> (fyx `App` gy) = ((.) <$> fxa <*> fyx) `App` gy -- Composition
 fxa     <*> (fyx :<*> fy)  = ((.) <$> fxa <*> fyx) :<*>  fy -- Composition

-- | @let Many{} = xs <|> ys@ always succeeds
-- lawless?
-- pattern Empty = Many []
instance Functor f => Alternative (Alt f) where
 empty = Many []
 (Many []) <|> ys        = ys              -- Left-Identity
 xs        <|> (Many []) = xs              -- Right-Identity
 (Many xs) <|> (Many ys) = Many (xs <> ys) -- Associativity?
 xs        <|> ys        = many_ xs <|> many_ ys
  where
  many_ :: Alt f a -> Alt f a
  many_ (Many xs) = Many xs
  many_ x         = Many [x]

lift :: f a -> Alt f a
lift ga = Pure id `App` ga

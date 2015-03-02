{-# LANGUAGE DeriveFunctor, DeriveTraversable, GADTs, InstanceSigs #-}
{-# LANGUAGE TypeFamilies                                          #-}
-- | (see <https://hackage.haskell.org/package/free-4.10.0.1/docs/Control-Alternative-Free.html Control.Alternative.Free> for inspiration)

module Control.Alternative.Free.Johansen where
import           Control.Applicative
import           Data.Foldable       (Foldable (..))
import qualified Data.Foldable       as Foldable
import           Data.Monoid
import           Data.Traversable    (Traversable)
import           GHC.Exts            (IsList (..))

-- $setup
-- >>> import Test.QuickCheck.Modifiers (Blind(..))


-- | Applicatives and Alternatives
data Tree a = Leaf a | Branch [Tree a]
 deriving (Show, Eq, Ord, Functor, Traversable)


-- | a "breadth-first" foldr.
--
-- the default implementation of 'foldr' wrt 'foldMap' uses the 'Endo' 'Monoid'.
--
-- supports short-circuiting (e.g. in infinite trees of infinitely-many alternatives) by being "sufficiently lazy".
instance Foldable Tree where
 -- foldr :: (a -> b -> b) -> b -> Tree a -> b
 -- foldr f b (Leaf a) = f a b
 -- foldr f b as = foldr f b (map (foldr f b) as)  -- TODO multiple b necessary? neutral only with Monoids
 foldMap :: Monoid m => (a -> m) -> Tree a -> m
 foldMap f (Leaf a)    = f a <> mempty
 foldMap f (Branch as) = fold $ fmap (foldMap f) as

-- |
--
-- compare the (flat, un-associated) list Applicative:
--
-- >>> [(+1), (*10)] <*> [1,2,3] :: [Int]
-- [2,3,4,10,20,30]
--
-- against the (un-distributed, associated) tree Applicative:
--
-- >>> fromList [(+1), (*10)] <*> fromList [1,2,3] :: Tree Int
-- Branch [Branch [Leaf 2,Leaf 3,Leaf 4],Branch [Leaf 10,Leaf 20,Leaf 30]]
--
-- you can recover the unassociated behavior by collapsing the associations with 'toList':
--
-- prop> \(Blind fs) xs -> (fs <*> xs) == toList (fromList fs <*> fromList xs :: Tree Int)
--
--
instance Applicative Tree where
 pure = Leaf
 Leaf f     <*> xs        = f `fmap` xs                 -- Identity (shape of xs)
 fs         <*> Leaf x    = ($ x) `fmap` fs             -- Interchange (shape of fs)
 Branch fs  <*> xs        = Branch $ (<*> xs) `fmap` fs -- (shape of fs)

-- | 'mappend' mostly preserves the depth of the inputs
instance Monoid (Tree a) where
 mempty = Branch []
 Leaf x    `mappend` Leaf y    = Branch ([Leaf x] <> [Leaf y])
 Leaf x    `mappend` Branch ys = Branch ([Leaf x] <> ys)
 Branch xs `mappend` Leaf y    = Branch (xs       <> [Leaf y])
 Branch xs `mappend` Branch ys = Branch (xs       <> ys)

 -- xs `mappend` ys = Branch [xs, ys]
-- not associative the way you want to do it. Maybe the associative version screws The foldable instance up?

instance IsList (Tree a) where
 type Item (Tree a) = a
 fromList = fromLeaves
 toList   = Foldable.toList

-- |
-- >>> fromLeaves [1,2,3]
-- Branch [Leaf 1,Leaf 2,Leaf 3]
--
fromLeaves :: [a] -> Tree a
fromLeaves = Branch . map Leaf

-- | Johansen-esque Free Alternative, i.e.:
--
-- * left-associated
-- * topmost-is-rightmost
-- * no @flip@ping (argument order is preserved)
-- * no @uncurry@ing/@curry@ing
--
-- luckily, the property I want (i.e. topmost-is-rightmost, which is not implied by left-associated) also allows the most natural definition (i.e. no flipping or uncurrying). (see "Commands.Parse.gparser"). that is, while the context-sensitive parser induced by a free-alternative-based grammar parses left to right, the context is threaded right to left. thus, the parser-generator is evaluated right to left: it peaks at, and evaluates, the rightmost grammar first; only once it has generated the parser from the rightmost grammar, can it generate the parser of the grammar to the left.
--
-- store the 'alternatives' as a 'Tree', not a list:
--
-- * the associativity (of '<|>') is preserved and explicit
-- * the distributivity (of '<|>' over '<*>') is "undistributed"/"breadth-first", not "distributed"/"depth-first"
--
--     * this matters with infinitely-many alternatives, which motivated this design
--     * we want to be able to evaluate possibly-infinite alternative-expressions by "short-circuiting", rather than the evaluation not terminating by "flattening". e.g.
--
--          finitely:
--
--          @
--          x && (y || z) = (x && y) || (x && z)
--          @
--
--          but infinitely, for example:
--
--          @
--          False && (False || False || ...) ≠ (False && False) || (False && False) || ...
--          @
--
--          because the left one terminates (@== False@) while the right one doesn't.
--
--          This example is for intuition; an equivalent situation shows up from recursively-defined parsers. e.g. (in pseudo-DSL):
--
--          @
--          command = (Undo \<$ "undo") \<|> (Repeat \<$> number \<*> command)
--          @
--
--          where a failing parse is equivalent to @False@ and '<*>' is equivalent to @&&@.
--
--
-- __tl;dr__ lists are flat trees; handle infinitely-many alternatives with a 'Tree' of 'Alternative's.
--
newtype Alt f a = Alt { alternatives :: Tree (App f a) }

instance Functor f => Functor     (Alt f) where
 fmap f (Alt xs) = Alt (fmap (fmap f) xs)

-- | fs is a Tree..
instance Functor f => Applicative (Alt f) where
 pure x = Alt (Leaf (pure x))
 -- Alt []  <*> _          = empty                                  -- empty is left annihilator for <*>
 -- _       <*> Alt []     = empty                                  -- empty is right annihilator for <*>
 -- Alt [f] <*> Alt [x,y]  = Alt [f <*> x] <|> Alt [f <*> y]        -- <*> left-distributes over <|>
 Alt fs <*> Alt xs = Alt $ fmap (<*>) fs <*> xs

 -- Alt fs <*> Alt xs = Alt $ (inject . (`apply` xs)) `map` fs
 --  where
 --  apply :: App f (x -> a) -> [App f x] -> Alt f a
 --  apply f xs = (f <*>) `map` xs
 --  inject :: Alt f a -> App f a
 --  inject f = _ f `App` _

-- prop> (length.alternatives) (Alt f <*> Alt x) = (length.alternatives) (Alt f)
-- (f <*>) :: App f a -> App f b

-- _ :: Tree (App f (a -> b)) -> Tree (App f a -> App f b)
-- (<*>) :: App f (a -> b) -> (App f a -> App f b)
-- fmap (<*>) :: f (App f (a -> b)) -> f (App f a -> App f b)

-- | The identity/associativity 'Alternative' laws for 'Alt' come from
-- the 'Monoid' laws for 'Tree'.
--
-- the distributivity law is TODO
instance Functor f => Alternative (Alt f) where
 empty = Alt mempty
 -- Alt [] <|> Alt ys = Alt ys  -- empty is left identity for <|>
 -- Alt xs <|> Alt [] = Alt xs  -- empty is right identity for <|>
 Alt xs <|> Alt ys = Alt (xs <> ys)


-- |
--
-- mutually-recursive with 'Alt'.
data App f a where
 Pure ::                     a -> App f a
 App  :: Alt f (x -> a) -> f x -> App f a

instance Functor f => Functor     (App f) where
 fmap f (Pure a) = Pure (f a)
 fmap f (gxa `App` hx) = fmap (f .) gxa `App` hx

instance Functor f => Applicative (App f) where
 pure = Pure
 Pure ab <*> fa             = fmap ab fa      -- Functor
 fab     <*> Pure a         = fmap ($ a) fab  -- Interchange
 fab     <*> (gxa `App` hx) = ((.) <$> Alt (Leaf fab) <*> gxa) `App` hx -- Composition

-- Homomorphism?

-- Identity:
-- pure id <*> x = Pure id <*> x = fmap id x = x

-- |
--
--
lift :: f a -> Alt f a
lift x = Alt $ Leaf (Alt (Leaf (Pure id)) `App` x) -- Alt [Alt [Pure id] `App` x]

-- |
--
--
fell :: Alternative f => Alt f a -> f a
fell = undefined


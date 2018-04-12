{-# LANGUAGE AutoDeriveTypeable, ConstraintKinds, DataKinds, DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric, ExistentialQuantification, FlexibleContexts     #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, RankNTypes, RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell, TypeOperators                                 #-}
-- | assorted functionality, imported by most modules in this package.
module Commands.Extra
 ( module Commands.Extra
 , module Prelude.Spiros

 , module Data.Possibly
 , module Data.Address
 , module Data.GUI
 , module Data.Data
 , module Data.HTypes
 , module Commands.Instances

 ) where
import           Commands.Instances
import Data.Possibly
import Data.Address
import Data.GUI
import Data.HTypes(Exists(..), exists)

import           Control.Lens                 (Lens', Prism', lens, prism')
import           Data.Hashable
import           Control.Monad.Reader         (ReaderT, local)
import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.Text.Lazy               (Text)
import           Numeric

-- TODO import Data.Functor.Classes
import           Control.Applicative
import           Control.Exception            (Exception (..), Handler,
                                               SomeException (..), catches, evaluate)
import           Data.Graph
import qualified Data.Set as Set
import           Data.List                    (nub)
import           Data.Maybe
import           Data.Ord
import           Data.Data
import qualified Debug.Trace
import           GHC.Exts                          (IsString (..))
import Control.Concurrent.STM(swapTVar,TVar,STM)
import System.Mem.StableName

import Prelude.Spiros hiding
 (Handler,catches,Possibly,throwM
 ,GUI(..),fromGlobalName
 )

import Prelude()

-- | logical implication as Boolean propositions. makes reading validators easier. read @p --> q@ it as "p implies q".
(-->) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p --> q = ((||) <$> (not . p) <*> q)

trace :: String -> a -> a
trace = Debug.Trace.trace

traced :: (Show a) => a -> a
traced = Debug.Trace.traceShowId

tracing :: (Show a, Monad m) => a -> m ()
tracing = Debug.Trace.traceShowM

with :: Monad m => r -> ReaderT r m a -> ReaderT r m a
with = local . const

hashAlphanumeric :: (Hashable a) => a -> String
hashAlphanumeric = flip showHex "" . abs . hash

nonemptyHead :: Lens' (NonEmpty a) a
nonemptyHead = lens
 (\(x :| _)    -> x)
 (\(_ :| xs) x -> x :| xs)

nonemptyTail :: Lens' (NonEmpty a) [a]
nonemptyTail = lens
 (\(_ :| xs)   -> xs)
 (\(x :| _) xs -> x :| xs)

-- | see <https://hackage.haskell.org/package/lens-4.7/docs/Control-Exception-Lens.html#g:6 Control.Exception.Lens>
prismException :: (Exception e) => Prism' SomeException e
prismException = prism' SomeException fromException

-- | @handles = flip 'catches'@
handles :: [Handler a] -> IO a -> IO a
handles = flip catches

type Adjacency k n = (n, k, [k])

{- | the maximal cycles of a directed graph (represented as an adjacency list)

wraps 'stronglyConnComp'

>>> :{
let graph = [ ("non recursive",        "N", [])
            , ("self recursive",       "S", ["S"])
            , ("mutually recursive A", "A", ["B"])
            , ("mutually recursive B", "B", ["A","C"])
            , ("mutually recursive C", "C", ["A","S","N"])
            ]
:}

>>> cycles graph
[["self recursive"],["mutually recursive A","mutually recursive B","mutually recursive C"]]

properties:

* the output @[[n]]@ is disjoint i.e. the cycles are maximal
* each output element @n@ comes from the input @Graph n e@ (but not the converse e.g. the output can be empty)
* when input an acyclic graph, the empty list is output (a singleton means the node has an edge to itself i.e. self-recursion) (in particular, the empty graph, lists, trees, DAGs)
* when input a complete graph, the singleton list of (the list of) vertices is output

TODO verify:

@
\as -> Set.fromList (map fst $ as) == Set.fromList (flattenSCCs . stronglyConnComp $ as)
@

i.e. it preserves the exact input nodes. which means that the partial 'find' can be safely assumed total.


-}
cycles :: Ord k => [Adjacency k n] -> [[n]]
cycles = sccs2cycles . stronglyConnComp

sccs2cycles :: [SCC n] -> [[n]]
sccs2cycles = mapMaybe $ \case
 AcyclicSCC _ -> Nothing
 CyclicSCC ns -> Just ns

-- | a natural transformation
type (:~>:) f g = forall x. f x -> g x

{- | left-biased maximum.

>>> 'maximumBy' ('comparing' 'snd') [('a',0),('b',0),('c',0)]
('c',0)
>>> maximumByL (comparing snd) [('a',0),('b',0),('c',0)]
('a',0)

-}
maximumByL :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumByL f = foldl1 pick
 where
 pick x y = case f x y of
  GT -> x
  EQ -> x
  LT -> y

{- |

>>> argmax length ["0","abc",""]
"abc"

-}
argmax :: Ord b => (a -> b) -> (NonEmpty a -> a)
argmax f = maximumByL (comparing f)

{- | interleave the effects of @g@ between the effects of @f@, collecting the results of @f@.
-}
interleaving :: (Alternative f) => f a -> f x -> f (NonEmpty a)
interleaving f g = (:|) <$> f <*> many (g *> f)

{- | for convenience when writing string dicts, let's you make a value equal to its key.

@
 [ "a"-: "b"
 , "b"-: "a"
 , both "c"
 ]
@

evaluates to:

@
 [ "a"-: "b"
 , "b"-: "a"
 , "c"-: "c"
 ]
@

-}
both :: a -> (a, a)
both a = (a, a)

{- | for convenience when writing string dicts, lets you leave keys/values blank

@
filterBlanks
 [ "a"-: "b"
 , "b"-: "a"
 , ""-: "..."
 ]
@

evaluates to:

@
 [ "a"-: "b"
 , "b"-: "a"
 ]
@

-}
filterBlanks :: (IsString k, Eq k) => [(k, v)] -> [(k, v)]
filterBlanks = filter $ \case
 ("",_) -> False
 (_, _) -> True

{- | n-ary (homogeneous) depth-first cross-product.

>>> let xss = [[1,2,3],[4,5,6]]
>>> cross xss
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
>>> length (cross xss)
9
>>> map length (cross xss)
[2,2,2,2,2,2,2,2,2]
>>> length xss
2

@cross = 'sequence'@

prop> length (cross xss) == product (map length xss)

prop> all (\ys -> length xss == length ys) (cross xss)

-}
cross :: [[a]] -> [[a]]
cross = sequence

{-| "dirty" the cache.

-}
takeTVar :: TVar (Maybe a) -> STM (Maybe a)
takeTVar var = swapTVar var Nothing

-- |
forceStableName
 :: (MonadIO m)
 => a -- ^ strict in @a@
 -> m (StableName a)
forceStableName x = liftIO (evaluate x) >> liftIO (makeStableName x)

newStableName
 :: (MonadIO m)
 => a -- ^ strict in @a@
 -> m (StableName a)
newStableName x = liftIO $ do
  _ <- evaluate x
  makeStableName x

-- | remove duplicates in @O(n log n)@ time.
-- <https://github.com/nh2/haskell-ordnub>
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

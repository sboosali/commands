{-# LANGUAGE GADTs, FlexibleContexts, UndecidableInstances #-}
-- | (see <https://hackage.haskell.org/package/free-4.10.0.1/docs/Control-Alternative-Free.html Control.Alternative.Free> for inspiration)

module Control.Alternative.Free.Johansen where
import Control.Applicative


-- | Johansen-esque Free Alternative, i.e.:
--
-- * left-associated
-- * topmost-is-rightmost
-- * argument-order-preserved
--
-- luckily, the property I want (i.e. topmost-is-rightmost) also allows the most natural definition (i.e. no flipping or uncurrying). (see "Commands.Parse.gparser"). that is, while the context-sensitive parser induced by a free-alternative-based grammar parses left to right, the context is threaded right to left. thus, the parser-generator is evaluated right to left: it peaks at, and evaluates, the rightmost grammar first; only once it has generated the parser from the rightmost grammar, can it generate the parser of the grammar to the left.
--
--
newtype Alt f a = Alt { alternatives :: [App f a] }

instance Functor f => Functor     (Alt f) where
 fmap f (Alt xs) = Alt (fmap (fmap f) xs)

instance Functor f => Applicative (Alt f) where
 pure x = Alt [pure x]
 -- Alt []  <*> _          = empty                                  -- empty is left annihilator for <*>
 -- _       <*> Alt []     = empty                                  -- empty is right annihilator for <*>
 -- Alt [f] <*> Alt [x,y]  = Alt (f <*> x) <|> Alt (f <*> y)        -- <*> left-distributes over <|>
 Alt fs  <*> Alt xs     = Alt (fmap (<*>) fs <*> xs) -- TODO the right order?
 -- (<*>) f :: App f a -> App f b

 -- Alt []  <*> _          = empty
 -- Alt [f]  <*> Alt xs     = Alt ([(f <*>)] <*> xs)
 -- Alt (f:fs)  <*> Alt xs     = (Alt [f] <*> Alt xs) <|> (Alt fs <*> Alt xs)


-- >>> [(+1), (*10)] <*> [1,2,3]
-- [2,3,4,2,4,6]

instance Functor f => Alternative (Alt f) where
 empty = Alt []
 -- Alt [] <|> Alt ys = Alt ys  -- empty is left identity for <|>
 -- Alt xs <|> Alt [] = Alt xs  -- empty is right identity for <|>
 Alt xs <|> Alt ys = Alt (xs ++ ys)


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
 Pure ab <*> fa           = fmap ab fa      -- fmap
 fab     <*> Pure a       = fmap ($ a) fab  -- interchange
 fab     <*> gxa `App` hx  = ((.) <$> Alt [fab] <*> gxa) `App` hx

-- |
--
--
lift :: f a -> Alt f a
lift x = Alt [Alt [Pure id] `App` x]

-- |
--
--
fell :: Alternative f => Alt f a -> f a
fell = undefined


instance Show (App f a) => Show (Alt f a) where
 show (Alt xs) = "Alt " ++ show xs

instance Show (App f a) where
 show (Pure _) = "Pure _"
 show (xs `App` _) = "(" ++ show xs ++ ") `App` _"


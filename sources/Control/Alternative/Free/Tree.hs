{-# LANGUAGE DeriveFunctor, GADTs, PatternSynonyms, TypeFamilies, RankNTypes, ScopedTypeVariables #-}
{- |

see <https://ro-che.info/articles/2013-03-31-flavours-of-free-applicative-functors.html flavours of free applicative functors> for different free applicatives, and certain properties (e.g. associativity, the complexity of certain operations, etc.).

see <https://hackage.haskell.org/package/free-4.10.0.1/docs/Control-Alternative-Free.html Control.Alternative.Free> for an example of a free alternative (which didn't satisfy certain properties I wanted, hence this module).


-}
module Control.Alternative.Free.Tree where

import Control.Applicative
-- import           Data.Foldable       (Foldable (..))
-- import qualified Data.Foldable       as Foldable
import Data.Monoid


{- |


= the 'Applicative' instance

mostly satisfies Applicative laws: identity and interchange and composition, sort of by construction, and ignoring Many; and homomorphism, possibly.

Explicitly violates (both left- and right-) distributivity (i.e. can't rewrite @(x + y) * z@ into @(x * z) + (y * z)@; nor can @x * (y + z)@ be rewritten into @(x * y) + (x * z)@), an Alternative law. the interpretation (i.e. a function of type @Alt f a -> _@) can distribute or not as needed.



Why? Because in @txa '<*>' 'Many' txs@, while @txs@ should always be
bounded, it may become unbounded under distribution. in analogy, the finite:

@False && (False || False)@ equals @(False && False) || (False && False)@

but, the infinite:

@False && (False || False || ...)@ doesn't equal @(False && False) || ...@

because @False && (False || False || ...)@ evaluates to @False@
whereas @(False && False) || ...@ never terminates.

then, reading:

* @&&@ as @'<*>'@
* @||@ as @'<|>'@
* @False@ as a "failing computation"

we can understand that by rejecting arbitrary distributivity our free alternative can preserve short-circuiting of recursively-defined parser-combinators. that is, we can't distribute over lists, because Haskell lists are really streams i.e. possibly infinite. the natural and law-preserving implementation of free alternatives does a depth first search, with the applicative instance of lists (e.g. @[(+1), (*10)] <*> [1,2,3] == [2,3,4,10,20,30]@). when combining infinities, @(f <|> g <|> ...) <*> (x <|> y <|> ...) = (f <*> x) <|> (f <*> y) <|> ...)@, you never reach the second alternative @g@, and thus lose short-circuiting.

At least, I did, when writing a parser generator for a grammar (of
type free alternative).

(we have infinite "sum"s, but not infinite "product"s: because @<|>@ preserves the type of its arguments, but @<*>@ changes the type, and you can't have an infinite type. which is why @<*>@ doesn't really seem like "multiplication" to me, but it helps me think about all this).


= the 'Alternative' instance

satisfies the monoid laws of identity and associativity.

satisfies annihilation, but not necessarily distributivity. (see above)

also, @let 'Many'{} = xs '<|>' ys@ succeeds unless both @xs@ and @ys@ are non-@Many@.

(so.. I think we have an algebraic structure that is a monoid in two ways ("addition" with "zero", and "multiplication" with "one"), related by an annihilator ("zero" under "multiplication") but not by distributivity. Don't know what it's called, or if it matters, but watch out.)


= Notes on Implementation:

* ':<*>' is necessary to __not distribute__ in the 'Many' case of the Applicative instance: @txa '<*>' 'Many' txs = txa ':<*>' 'Many' txs@
* while specific values of type @Alt f a@ may be mostly "list-like" (i.e. those without 'Many'), without distributing, they are be "tree-like" generally.


= Notes on Naming (in the source):

* no prefix means a value of a pure type
* the "f" prefix means a value of a functorial (?) type
* the "t" prefix means a value of an alternative type
* the "s" suffix means a value of a list type
* the remaining "infix" means a value of any type or an arrow type

like:

* @a  :: a@
* @fa :: f a@
* @ta :: Alt f a@
* @tas :: [Alt f a]

and:

* @xa  :: (x -> a)@
* @fxa :: f (x -> a)@
* @txa :: Alt f (x -> a)@
* @txas :: [Alt f (x -> a)]

(why? easier than the reader/writer doing type inference in their head).


= Related

* <http://stackoverflow.com/questions/15722906/must-mplus-always-be-associative-haskell-wiki-vs-oleg-kiselyov/15853770#15853770>

* <http://winterkoninkje.dreamwidth.org/90905.html>



-}
data Alt f a where
 Pure   ::                         a -> Alt f a
 Many   ::                 [Alt f a] -> Alt f a
 App    :: Alt f (x -> a) -> f x     -> Alt f a
 (:<*>) :: Alt f (x -> a) -> Alt f x -> Alt f a

instance Functor f => Functor     (Alt f) where
 fmap ab (Pure a)       = Pure (ab a)
 fmap ab (Many fas)     = Many (fmap (fmap ab) fas)
 fmap ab (txa `App` fx) = fmap (ab .) txa `App` fx
 fmap ab (txa :<*>  tx) = fmap (ab .) txa :<*>  tx

instance Functor f => Applicative (Alt f) where
 pure  = Pure

 Pure xa <*> tx             = fmap xa tx                     -- Functor
 txa     <*> Pure x         = fmap ($ x) txa                 -- Interchange

 _       <*> Empty          = Empty                          -- Annihilation
 txa     <*> Many txs       = txa :<*> Many txs              -- not Distributivity

 txa     <*> (tyx `App` fy) = ((.) <$> txa <*> tyx) `App` fy -- Composition
 txa     <*> (tyx :<*> ty)  = ((.) <$> txa <*> tyx) :<*>  ty -- Composition

instance Functor f => Alternative (Alt f) where
 empty = Many []

 Empty     <|> ys        = ys              -- Left-Identity
 xs        <|> Empty     = xs              -- Right-Identity
 (Many xs) <|> (Many ys) = Many (xs <> ys) -- Associativity

 xs        <|> ys        = Many (toAltList xs) <|> Many (toAltList ys)

toAltList :: Alt f a -> [Alt f a]
toAltList (Many xs) = xs
toAltList x         = [x]

pattern Empty = Many []

liftAlt :: f a -> Alt f a
liftAlt f = Pure id `App` f

fellAlt :: (Alternative f) => Alt f a -> f a
fellAlt = runAlt id

-- | 'Alt' satisfies associativity modulo 'runAlt'. (I think)
runAlt :: forall f g a. Alternative g => (forall x. f x -> g x) -> Alt f a -> g a
runAlt _ (Pure a)     = pure a
runAlt u (Many fs)    = foldr (<|>) empty (runAlt u <$> fs)
runAlt u (f `App` x) = runAlt u f <*> u x
runAlt u (f :<*>  g) = runAlt u f <*> runAlt u g


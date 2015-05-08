{-# LANGUAGE DeriveFunctor, GADTs, LambdaCase, LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes, StandaloneDeriving, TypeOperators         #-}
module Commands.Symbol.Types where

import Data.Functor.Coyoneda
import Data.List.NonEmpty

import Control.Applicative
import Data.Monoid


{- | a rule pairs, conceptually, a left-hand side with a right hand side. the left-hand side lets us reify definitions, including recursive ones.

the type parameters:

* @l@ for "left"-hand side
* @i@ for "inputted" tokens
* @p@ for "parser"
* @r@ for "reified" grammar

when written separately rather than automatically derived, the '_ruleParser' should be consistent with the '_ruleReified'.
where the notion of consistency is determined by the interaction between them (e.g. parsing recognitions of the grammar from a external speech recognition engine)

every type is abstract, for extensibility.

-}
data Rule l i p r a = Rule
 { _ruleLHS     :: l            -- ^ uniquely-identifying: used to terminate recursion (e.g. when reifying a rule into a graph).
 , _ruleReified :: r i l        -- ^ a non-recursive grammar that reifies the rule, whose non-terminals are tagged with @l@ and terminals are @i@.
 , _ruleParser  :: p i a        -- ^ a parser that consumes @i@'s and produces an @a@. must be a Functor.
 }
 deriving (Functor)

{- | a right-hand side.

an Alternative, as @'Alter' f@ is the "free alternative" of any functor @f@.

'RHS' and 'Rule' and (this specialized) 'Symbol' are conceptually mutually recursive:

* 'RHS' *will* hold a @'Symbol' ('Rule' _) _@
* @'Symbol' ('Rule' _) _@ *may* hold a 'Rule' (in the 'NonTerminal' case)
* a 'Rule' is derived from (but *doesn't* hold) an 'RHS'

but actually, you manipulate the 'RHS' with 'Symbol's\' instances, and at each "level", the 'RHS' is erased away into a '_ruleGrammar' and '_ruleParser'.

-}
type RHS l i p r = Alter (SymbolF i (Rule l i p r))

-- | see 'runAlter'
runRHS
 :: Alternative f
 => (forall x. Symbol i (Rule l i p r) x -> f x)
 -> RHS l i p r a
 -> f a
runRHS f = runAlter (\(Coyoneda g x) -> fmap g (f x))

-- | see 'foldAlter'
foldRHS
 :: (forall x. Symbol i (Rule l i p r) x -> b) -- ^ @b@ can't contain @x@, but can contain the rest
 -> b
 -> (b -> b -> b)
 -> b
 -> ([b] -> b)
 -> RHS l i p r a
 -> b
foldRHS f = foldAlter (\(Coyoneda _ x) -> f x)

{- | a Functor, as @'Coyoneda' f@ is the "free functor" of any (!) type constructor @f@.

-}
type SymbolF t n = Coyoneda (Symbol t n)

{- | a grammatical symbol for <https://en.wikipedia.org/wiki/Extended_Backus-Naur_Form EBNF>.

the type parameters:

* @n@ for "name" or "non-terminal". when @n@ contains a @Symbol n t a@ somewhere (e.g. like), different 'Symbol's\' (represented as 'NonTerminal's) recursion is direct. when @n@ doesn't (e.g. @n ~ String@ or @n ~ Unique@), any recursion should be indirect. you should always explicitly reify recursion through 'NonTerminal's, however you represent the @n@.

* @t@ for "terminal" (e.g. @t ~ String@ for phrases tokenized into words)

the constructors:

* 'Terminal': @"..."@.
* 'NonTerminal': @<...>@.
* 'Opt': @(...)?@ i.e. zero or one of the thing. like Alternative's @optional@ function.
* 'Many': @(...)*@ i.e. zero or more of the thing. like Alternative's @many@ method.
* 'Some': @(...)+@ i.e. one or more of the thing. like Alternative's @some@ method.

EBNF features:

* "grouping" is comes from the AST.
* "alternation" comes from 'RHS's 'Alternative' instance.
* "sequencing" comes from 'RHS's 'Applicative' instance.
* "optionality" and "repetition" come from the constructors (i.e. 'Opt', 'Many', 'Some').
* "definition" comes from the 'Rule' product-type.
* *no* "exceptions"

this type is really more of a <http://bnfc.digitalgrammars.com/ labeled BNF>:

* the type of '<*>' is heterogeneous, which lets us capture non-terminals of different types
* a constructor on the left of '<$>' labels the sequence.

because the strictness annotation @!n@ only forces evaluation to weak-head-normal-form, Haskell's non-strictness should still let us safely build directly-recursive Symbols.

*not* a Functor, every constructor violates parametric polymorphism.

-}
data Symbol t n a where
  Terminal    :: !t              -> Symbol t n t            --
  NonTerminal :: !(n a)          -> Symbol t n (n a)        --
  Opt         :: !(Symbol t n a) -> Symbol t n (Maybe a)    --
  Many        :: !(Symbol t n a) -> Symbol t n [a]          --
  Some        :: !(Symbol t n a) -> Symbol t n (NonEmpty a) --

liftSymbol :: Symbol i (Rule l i p r) a -> RHS l i p r a
liftSymbol = liftAlter . liftCoyoneda

{- | a "free alternative".

the constructors:

* @(':<*>')@ is just a hack to embed an Alter on the right and not just the left: avoids left-distributing over possibly infinitely many alternatives. which, for example, would force any interpretation as a search to be depth first (where "interpretation" means the output of any function @interpret = 'runAlter' $ \case ...@). this is why I can't just use <https://hackage.haskell.org/package/free-4.10.0.1/docs/Control-Alternative-Free.html#t:Alt this free alternative>.

-}
data Alter f a where
  -- Applicative
  Pure        ::                                   a -> Alter f a
  Apply       :: !(Alter f (x -> a)) -> !(f x)       -> Alter f a
  (:<*>)      :: !(Alter f (x -> a)) -> !(Alter f x) -> Alter f a

  -- Alternative/Monoid
  Empty       ::                                        Alter f a
  Alter       ::                        ![Alter f a] -> Alter f a
 -- TODO? (NonEmpty Alter f a)
 -- TODO? pattern Empty = Alter []
 -- TODO? crystallize alternative methods:
  -- Opt         :: !(Alter f (Maybe x    -> a)) -> Alter f (Maybe x)    -> Alter f a
  -- Many        :: !(Alter f ([x]        -> a)) -> Alter f [x]          -> Alter f a
  -- Some        :: !(Alter f (NonEmpty x -> a)) -> Alter f (NonEmpty x) -> Alter f a
  -- optional is a function, not a method :-(

deriving instance (Functor (Alter f))

{- |

see the <http://hackage.haskell.org/package/base-4.7.0.2/docs/Control-Applicative.html#t:Applicative Applicative Laws>

see the source for "proofs" (most by construction, some derived).

intentionally violates left-distributivity:

@
f '<*>' (x '<|>' y) = f '<*>' 'Alter' [x, y] = f ':<*>' 'Alter' [x, y]

f <*> x <|> f <*> y = Alter [f <*> x, f <*> y]

f :<*> Alter [x, y] ≠ Alter [f <*> x, f <*> y]

f <*> (x <|> y) ≠ f <*> x <|> f <*> y
@

but when lawfully interpreted, for finitely-many alternatives, we should at least have:

@
'runAlter' u (f '<*>' (x '<|>' y)) = 'runAlter' u (f '<*>' x '<|>' f '<*>' y)

runAlter u (f :<*> Alter [x, y])
runAlter u f <*> runAlter u (Alter [x, y])
runAlter u f <*> foldr (<|>) empty (runAlter u <$> [x, y])
runAlter u f <*> foldr (<|>) empty [runAlter u x, runAlter u y]
runAlter u f <*> (runAlter u x <|> runAlter u y)
(runAlter u f <*> runAlter u x) <|> (runAlter u f <*> runAlter u y)  -- (runAlter u _) distributes lawfully by assumption

runAlter u (Alter [f <*> x, f <*> y])
foldr (<|>) empty (runAlter u <$> [f <*> x, f <*> y])
foldr (<|>) empty [runAlter u (f <*> x), runAlter u (f <*> y)]
runAlter u (f <*> x) <|> runAlter u (f <*> y)
... TODO finish proof
(runAlter u f <*> runAlter u x) <|> (runAlter u f <*> runAlter u y)

@

-}
instance Functor f => Applicative (Alter f) where
 pure = Pure

 Pure xa <*> tx                = fmap xa tx                       -- Functor
 -- Pure {id} <*> x            = fmap {id} x     = x              -- Identity
 -- Pure f <*> Pure x          = fmap f (Pure x) = Pure (f x)     -- Homomorphism
 txa     <*> Pure x            = fmap ($ x) txa                   -- Interchange

 _       <*> Empty             = Empty                            -- Annihilation
 txa     <*> Alter txs         = txa :<*> Alter txs               -- NOT Distributivity

 txa     <*> (tyx `Apply` fy)  = ((.) <$> txa <*> tyx) `Apply` fy -- Composition
 txa     <*> (tyx :<*>    ty)  = ((.) <$> txa <*> tyx) :<*>    ty -- Composition

-- TODO finish proof
--
-- I've gotta case analyze on:
--
-- Pure f' <*> x              = fmap f' x
-- f     <*> Pure x'          = fmap ($ x') f
-- _     <*> Empty            = Empty
-- f     <*> Alter xs'        = f :<*> Alter xs'
-- f     <*> (g' `Apply` i')  = ((.) <$> f <*> g') `Apply` i'
-- f     <*> (g' :<*>    x')  = ((.) <$> f <*> g') :<*>    x'
--
-- or must I?

{- |

see the <http://hackage.haskell.org/package/base-4.7.0.2/docs/Control-Applicative.html#t:Alternative Alternative Laws>

doesn't use its @(Functor f)@ constraint, because we don't left-distribute sequencing (i.e. '<*>') over alternation (i.e. '<|>').

when neither @x@ nor @y@ are 'Empty', then:

@
let Alter zs = x <|> y
@

successfully pattern-matches.

-}
instance Functor f => Alternative (Alter f) where
 empty = Empty

 Empty <|> y = y                                  -- Left-Identity
 x <|> Empty = x                                  -- Right-Identity
 x <|> y = Alter (toAlterList x <> toAlterList y) -- Associativity

toAlterList :: Alter f a -> [Alter f a]
toAlterList (Alter xs) = xs
toAlterList Empty      = []
toAlterList x          = [x]

-- |
liftAlter :: f a -> Alter f a
liftAlter f = Pure id `Apply` f

{- |

left-inverse to 'liftAlter':

@
'fellAlter' ('liftAlter' f)
'runAlter' 'id' ('Pure' id `'App'` f)
runAlter id (Pure id) '<*>' id f
pure id '<*>' f
f
@

-}
fellAlter :: (Alternative f) => Alter f a -> f a
fellAlter = runAlter id

{- | fold an Alter down to a value, by acting on the functor.

like 'runAlter' where @(g ~ Const b)@ but not, as 'Const' is not 'Alternative'.

-}
foldAlter
 :: (forall x. f x -> b) -- ^ how to fold the functor
 -> b             -- ^ a @one@, i.e. how to interpret 'pure _'
 -> (b -> b -> b) -- ^ a @multiply@, i.e. how to interpret '<*>'
 -> b          -- ^ a @zero@, i.e. how to interpret 'empty'
 -> ([b] -> b) -- ^ a @sum@mation, i.e. how to interpret '<|>'
 -> Alter f a
 -> b
foldAlter u one multiply zero sum = \case
 Pure _      -> one
 f `Apply` x -> foldAlter u one multiply zero sum f `multiply` u x
 f :<*>  g   -> foldAlter u one multiply zero sum f `multiply` foldAlter u one multiply zero sum g
 Empty       -> zero
 Alter fs    -> sum $ fmap (foldAlter u one multiply zero sum) fs

foldAlterMonoid
 :: Monoid m             -- ^ '<>' interprets '<*>', and 'mempty' interprets 'pure' (like <http://hackage.haskell.org/package/base-4.8.0.0/docs/src/Control-Applicative.html#line-84 instance Monoid m => Applicative (Const m)>)
 => (forall x. f x -> m)
 -> m
 -> ([m] -> m)
 -> Alter f a
 -> m
foldAlterMonoid f = foldAlter f mempty mappend

{- | interpret an Alter as some Alternative, by acting on the functor.

'Alt' satisfies associativity modulo 'runAlt' (I think).

-}
runAlter
 :: Alternative g
 => (forall x. f x -> g x)
 -> Alter f a
 -> g a
runAlter _ (Pure a)      = pure a
runAlter u (f `Apply` x) = runAlter u f <*> u x
runAlter u (f :<*>    g) = runAlter u f <*> runAlter u g
runAlter _ Empty         = empty
runAlter u (Alter fs)    = foldr (<|>) empty (runAlter u <$> fs)


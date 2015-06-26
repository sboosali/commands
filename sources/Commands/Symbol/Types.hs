{-# LANGUAGE DeriveFunctor, FlexibleContexts, GADTs, LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms, PatternSynonyms, RankNTypes   #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, TypeOperators #-}
module Commands.Symbol.Types where

import           Control.Lens
import           Data.Functor.Coyoneda
import           Data.List.NonEmpty(NonEmpty (..))
import qualified Data.List.NonEmpty    as NonEmpty

import           Control.Applicative
import           Data.Monoid
import           Data.Foldable                 (asum)




{- | a command pairs a rule with

the type parameters:

* @d@ for "desugarer"
* @b@ for "bytecode", the desugared

e.g. a simple specialization:

@
Command _ _ Op _ _ (IO()) a
-- type Op b a = a -> b
@


-}
data Command p r d l i b a = Command
 { _comRule    :: Rule p r l i a
 , _comDesugar :: d b a          -- ^ how to desugar the \"AST" into the "bytecode". consumes @a@, producing @b@. should be contravariant.
 }

-- | e.g. @dimap :: ([Action] -> IO()) -> (Natural -> Integer) -> Command p r d l i (IO()) Natural -> Command p r d l i [Action] Integer@ i.e. "desugaring naturals to I/O actions" becomes "desugaring integers to lists of sandboxed actions".
instance (Profunctor d, Functor (p i)) => Profunctor (Command p r d l i) where
 dimap before after (Command rule d) = Command (fmap after rule) (dimap before after d)

{- | a rule (conceptually) pairs a left-hand side with a right hand side. the left-hand side lets us reify definitions, including recursive ones.

the type parameters:

* @l@ for "left"-hand side
* @i@ for "inputted" tokens
* @p@ for "parser"
* @r@ for "reified" grammar

when written separately rather than automatically derived, the '_ruleParser' should be consistent with the '_ruleReified'.
where the notion of consistency is determined by the interaction between them (e.g. parsing recognitions of the grammar from a external speech recognition engine)

every type is abstract, for extensibility.

e.g. a simple specialization:

@
Rule Parser Reified Unique String a
-- type Reified i l = Tree (Either i l)
-- type Parser i a = i -> Maybe a
@

-}
data Rule p r l i a = Rule
 { _ruleLHS     :: l            -- ^ uniquely-identifying: used to terminate recursion (e.g. when reifying a rule into a graph).
 , _ruleReified :: r l i        -- ^ a non-recursive grammar that reifies the rule, whose non-terminals are tagged with @l@ (and whose terminals are tagged with anything, possibly @i@ but not necessarily).
 , _ruleParser  :: p i a        -- ^ a parser that consumes @i@'s and produces an @a@. should be a (convariant) Functor.
 }
 deriving (Functor)

{- | use:

* as both the reification ('_ruleReified') and the left-hand side ('_ruleLHS') don't depend on the existentially-quantified parameter, they lose no info, and can be used "as is".
* existentially-quantified parsers ('_ruleParser') can be used
when the result is ignored (e.g. with '*>').
-}
data SomeRule p r l i = forall x. SomeRule (Rule p r l i x)

-- | 'Rule' is "invariant" in @i@.
--
-- e.g. @invmapRule :: (String -> Text) -> (Text -> String) -> Rule p r l String a -> Rule p r l Text a@
invmapRule
 :: (Functor (r l), Profunctor p)
 => (i -> i') -> (i' -> i) -> Rule p r l i a -> Rule p r l i' a
invmapRule into from (Rule l r p) = Rule l (fmap into r) (lmap from p)

-- | 'Rule' is "invariant" in @i@ and "covariant" in @a@.
--
--
-- e.g. @dimapRule :: (String -> Text) -> (Text -> String) -> (Natural -> Integer) -> Rule p r l String Natural -> Rule p r l Text Integer@
dimapRule
 :: (Functor (r l), Profunctor p)
 => (i -> i')
 -> (i' -> i)
 -> (a -> a')
 -> Rule p r l i a
 -> Rule p r l i' a'
dimapRule into from f (Rule l r p) = Rule l (fmap into r) (dimap from f p)

{- | a right-hand side in a
<https://en.wikipedia.org/wiki/Extended_Backus-Naur_Form EBNF>.

an Alternative, as @'Alter' f@ is the "free alternative" of any functor @f@.

EBNF features:

* "terminals" and "non-terminals" come from 'Symbol's constructors.
* "alternation" comes from 'RHS's 'Alternative' instance.
* "sequencing" comes from 'RHS's 'Applicative' instance.
* "optionality" and "repetition" come from 'Alter's constructors (i.e. 'Opt', 'Many', 'Some').
* "definition" comes from the 'Rule' product-type and the @induce*@ functions.
* "grouping" just comes from Haskell's precedence/parentheses.
* *no* "exceptions"

this type is really more of a <http://bnfc.digitalgrammars.com/ labeled BNF>:

* a constructor on the left of '<$>' labels the grammatical sequence.
* the type of '<*>' is heterogeneous, which lets us capture non-terminals of different types

'RHS' and 'Rule' and (specialized) 'Symbol' are conceptually mutually recursive:

* 'RHS' *will* hold a @'Symbol' ('Rule' _) _@
* @'Symbol' ('Rule' _) _@ *may* hold a 'Rule' (in the 'NonTerminal' case)
* a 'Rule' is derived from (but *doesn't* hold) an 'RHS'

but actually, you manipulate the 'RHS' with 'Symbol's\' instances, and at each "level", the 'RHS' is erased away into a '_ruleGrammar' and '_ruleParser'.

"inlining" the types, we almost get:

@
type RHS p r l i a = Alter (Coyoneda (Free Maybe (Either i (l, r _ _, p _ a))))
@

which shows that the outer types are just there to mess around with a grammatical production "tree" that holds terminals and non-terminals at its "leaves". (the @Free Maybe@ doesn't matter, it just provides some type with nesting but not branching). which I read as: "an Alternative on a Functor of Nested Terminals/NonTerminals".


-}
type RHS p r l i = Alter (SymbolF (Rule p r l) i)
-- newtype RHS p r l i = RHS (Alter (SymbolF (Rule p r l) i)) for Alternative instance where many = Many and some = Some? but optional still doesn't equal Opt.

-- -- | works on @'Symbol' a@, where @fmap@ would just work on @a@.
-- invmapRHS :: (b -> a) -> (Symbol (Rule p r l) i a -> Symbol (Rule p r l) i b) -> RHS p r l i a -> RHS p r l i b
-- invmapRHS f' f = runAlter (\(Coyoneda g x) -> (fmap g) (f x))
-- -- (liftSymbol . f)

-- | see 'runAlter'
runRHS
 :: Alternative f
 => (forall x. Symbol (Rule p r l) i x -> f x)
 -> RHS p r l i a
 -> f a
runRHS h = runAlter (\(Coyoneda g x) -> fmap g (h x))

-- | see 'foldAlter'
foldRHS
 :: (forall x. Symbol (Rule p r l) i x -> b) -- ^ @b@ can't (?) contain @x@, but can contain the rest
 -> b
 -> (b -> b -> b)
 -> ([b] -> b)
 -> (b -> b)
 -> (b -> b)
 -> (b -> b)
 -> RHS p r l i a
 -> b
foldRHS f = foldAlter (\(Coyoneda _ x) -> f x)

-- | see 'foldAlter_Monoid'
foldRHS_Monoid :: Monoid m => (forall x. Symbol (Rule p r l) i x -> m) -> RHS p r l i a -> m
foldRHS_Monoid f = foldAlter_Monoid (\(Coyoneda _ x) -> f x)

{- | a Functor, as @'Coyoneda' f@ is the "free functor" of any (!) type constructor @f@.

-}
type SymbolF n t = Coyoneda (Symbol n t)

{- | a grammatical symbol.

the type parameters:

* @n@ for "name" or "non-terminal". when @n@ contains a @Symbol n t a@ somewhere (e.g. like TODO), the recursion between different 'Symbol's (represented as 'NonTerminal's) is direct. when @n@ doesn't (e.g. @n ~ String@ or @n ~ Unique@), any recursion should be indirect. you should always explicitly reify recursion through 'NonTerminal's, however you represent the @n@.

* @t@ for "terminal" (e.g. @t ~ String@ for phrases tokenized into words)

because the strictness annotation @!n@ only forces evaluation to weak-head-normal-form, Haskell's non-strictness should still let us safely build directly-recursive Symbols.

\*not* a Functor, as a constructor ('Terminal') violates "parametric polymorphism" (when you try to write fmap).

-}
data Symbol n t a where
 Terminal    :: !t              -> Symbol n t t
 NonTerminal :: !(n t a)        -> Symbol n t a

liftSymbol :: Symbol (Rule p r l) i a -> RHS p r l i a
liftSymbol = liftAlter . liftCoyoneda

word :: i -> RHS p r l i i
word = liftSymbol . Terminal

rule :: (Rule p r l) i a -> RHS p r l i a
rule = liftSymbol . NonTerminal

-- opt :: RHS p r l i a -> RHS p r l i (Maybe a)
-- opt = runAlter (\(Coyoneda g x) -> liftAlter $ Coyoneda (fmap g) (Opt x))
-- -- opt = invmapRHS Opt

-- opt' :: RHS p r l i a -> a -> RHS p r l i a
-- opt' rhs a = fromMaybe a <$> opt rhs

-- many0 :: RHS p r l i a -> RHS p r l i [a]
-- many1 :: RHS p r l i a -> RHS p r l i (NonEmpty a)

{- | a "free alternative".

the constructors:

* @(':<*>')@ is just a hack to embed an Alter on the right and not just the left: avoids left-distributing over possibly infinitely many alternatives. which, for example, would force any interpretation as a search to be depth first (where "interpretation" means the output of any function @interpret = 'runAlter' $ \case ...@). this is why I can't just use <https://hackage.haskell.org/package/free-4.10.0.1/docs/Control-Alternative-Free.html#t:Alt this free alternative>.
* 'Many' for 'many'
* 'Some' for 'some'
* 'Opt' can't override 'optional', as @optional@ is a function, not a method. see 'optionalA'.

-}
data Alter f a where
 -- Applicative
 Pure        ::                                   a -> Alter f a
 Apply       :: !(Alter f (x -> a)) -> !(f x)       -> Alter f a
 (:<*>)      :: !(Alter f (x -> a)) -> !(Alter f x) -> Alter f a
 -- (:<*>)      :: !((x -> a)) -> !(Alter f x) -> Alter f a

 -- Alternative/Monoid
 Alter       ::                        ![Alter f a] -> Alter f a
 -- TODO? pattern Empty = Alter []
 -- TODO? (NonEmpty Alter f a)

 -- Coyoneda'd
 Opt         :: !(Maybe x    -> a) -> Alter f x -> Alter f a
 Many        :: !([x]        -> a) -> Alter f x -> Alter f a
 Some        :: !(NonEmpty x -> a) -> Alter f x -> Alter f a

pattern Empty = Alter []

deriving instance (Functor (Alter f))

instance (Functor f) => Monoid (Alter f a) where
 mempty = Empty
 mappend = (<|>)

{- |

see the <http://hackage.haskell.org/package/base-4.7.0.2/docs/Control-Applicative.html#t:Applicative Applicative Laws>

see the source for "proofs" (most by construction, some derived).

intentionally violates left-distributivity:

@
f '<*>' (x '<|>' y) = f '<*>' 'Alter' [x, y] = f ':<*>' 'Alter' [x, y]

f \<*> x \<|> f \<*> y = Alter [f \<*> x, f \<*> y]

f :\<*> Alter [x, y] ≠ Alter [f \<*> x, f \<*> y]

f \<*> (x \<|> y) ≠ f \<*> x \<|> f \<*> y
@

but when lawfully interpreted, for finitely-many alternatives, we should at least have:

@


runAlter u (f :\<*> Alter [x, y])
runAlter u f \<*> runAlter u (Alter [x, y])
runAlter u f \<*> foldr (\<|>) empty (runAlter u \<$> [x, y])
runAlter u f \<*> foldr (\<|>) empty [runAlter u x, runAlter u y]
runAlter u f \<*> (runAlter u x \<|> runAlter u y)
(runAlter u f \<*> runAlter u x) \<|> (runAlter u f \<*> runAlter u y)  -- (runAlter u _) distributes lawfully by assumption

runAlter u (Alter [f \<*> x, f \<*> y])
foldr (\<|>) empty (runAlter u \<$> [f \<*> x, f \<*> y])
foldr (\<|>) empty [runAlter u (f \<*> x), runAlter u (f \<*> y)]
runAlter u (f \<*> x) \<|> runAlter u (f \<*> y)
... TODO finish proof
(runAlter u f \<*> runAlter u x) \<|> (runAlter u f \<*> runAlter u y)

@

-}
instance Functor f => Applicative (Alter f) where
 pure = Pure

 Pure xa <*> tx                = fmap xa tx                       -- Functor
 -- Pure {id} <*> x            = fmap {id} x     = x              -- Identity
 -- Pure f <*> Pure x          = fmap f (Pure x) = Pure (f x)     -- Homomorphism
 txa     <*> Pure x            = fmap ($ x) txa                   -- Interchange

 Empty   <*> _                 = Empty                            -- left-Annihilation (?)
 _       <*> Empty             = Empty                            -- right-Annihilation
 txa     <*> Alter txs         = txa :<*> Alter txs               -- NO left-Distributivity

 txa     <*> (Opt  ysa ty)     = txa :<*> Opt  ysa ty -- TODO correct?
 txa     <*> (Many ysa ty)     = txa :<*> Many ysa ty
 txa     <*> (Some ysa ty)     = txa :<*> Some ysa ty

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

distinguishes between 'Some' and @(:) <$> x <*> 'Many' x@ (and vice versa). so these equations are violated:

@
some x = (:) <$> x <*> many x
many x = some x <|> pure []
@

but should be satisfied when lawfully interpreted...

1:

@
'runAlter' u ('many' x)
many (runAlter u x) -- (see below)
some (runAlter u x) <|> pure [] -- by assumption, (runAlter u x) is Alternative
some (runAlter u x) <|> runAlter u (Pure []) -- by definition of runAlter (TODO is it injective like that? maybe it must be trivial/unlawful when not injective?)
TODO
(id . NonEmpty.fromList) <$> some (runAlter u x)
NonEmpty.fromList <$> some (runAlter u x)
runAlter u (Some id x)
TODO
some x = fmap NonEmpty.toList (Some id x)
TODO
NonEmpty.fromList \<$> some (runAlter u x)
Some id x
TODO
runAlter u (fmap NonEmpty.toList (Some id x)) <|> runAlter u (Pure []) --
runAlter u (fmap NonEmpty.toList (Some id x) <|> Pure []) -- by distribution (see below)
runAlter u (some x <|> pure []) -- by definitions of some and pure
@

2:

@
TODO
'NonEmpty.fromList' '<$>' 'runAlter' ('some' x)
NonEmpty.fromList \<$>  ((:) <$> x <*> many x)
NonEmpty.fromList \<$>
NonEmpty.fromList \<$>
@

given:

@
runAlter u (many x)
runAlter u (Many id x) -- by definition
id \<$> many (runAlter u x) -- by definition
many (runAlter u x) -- by Functor identity
@

and:

@
runAlter u (some x)
runAlter u (Some id x) -- by definition
(id . 'NonEmpty.fromList') \<$> some (runAlter u x) -- by definition
NonEmpty.fromList \<$> some (runAlter u x) -- identity
@

and:

@
runAlter u (f <|> g)
runAlter u (Alter [f, g]) -- by definition
foldr (<|>) empty (runAlter u <$> [f, g]) -- by a case of runAlter (not bidirectional)
foldr (<|>) empty [runAlter u f, runAlter u g]
runAlter u f <|> runAlter u g
@

and:

@ TODO run commutes with fmap
runAlter u (fmap f x)
TODO
fmap f (runAlter u x)
@

when neither @x@ nor @y@ are 'Empty', then:

@
let Alter zs = x \<|> y
@

successfully pattern-matches.

the cast:

@
fmap 'NonEmpty.fromList' . ('some' :: Alter f a -> Alter f [a])
@

is safe:

@
fmap NonEmpty.fromList . (some :: Alter f a -> Alter f [a])
fmap NonEmpty.fromList . fmap 'NonEmpty.toList' . 'Some' id  -- by definition
fmap (NonEmpty.fromList . 'NonEmpty.toList') . 'Some' id  -- by homomorphism (Alter is a Functor)
fmap id . Some id  -- (see below)
Some id  -- fmap id = id
@

since:

@
(\\(a:as) -> a :| as) . (\\(a :| as) -> a : as)
\\(a :| as) -> (\\(a:as) -> a :| as) ((\\~(a :| as) -> a : as) (a :| as))   -- by definition of compose
\\(a :| as) -> (\\(a:as) -> a :| as) (a : as)   -- application
\\(a :| as) -> (a :| as)   -- application
(id :: NonEmpty a -> NonEmpty a)  -- NonEmpty is a product-type
id
@

-}
instance Functor f => Alternative (Alter f) where
 empty = Empty

 Empty <|> y = y                                  -- Left-Identity
 x <|> Empty = x                                  -- Right-Identity
 x <|> y = Alter (toAlterList x <> toAlterList y) -- Associativity
 {-# INLINE (<|>) #-}

 many = Many id
 {-# INLINE many #-}
 some = fmap NonEmpty.toList . Some id
 {-# INLINE some #-}

{-# RULES "NonEmpty.fromList some/Alter"  forall x.  fmap NonEmpty.fromList (fmap NonEmpty.toList (Some id x))  =  Some id x
  #-}
 -- TODO safe with bottom? at least, when x is bottom, (Some id x) is bottom too, being a strict constructor.

 -- (the pragmas are just for fun)

-- | a reified 'optional' for the Free 'Alter'native.
optionalA :: Alter f a -> Alter f (Maybe a)
optionalA = Opt id
{-# INLINE optionalA #-}

optionA :: a -> Alter f a -> Alter f a
optionA x = Opt (maybe x id)
{-# INLINE optionA #-}

toAlterList :: Alter f a -> [Alter f a]
toAlterList (Alter xs) = xs
toAlterList x          = [x]
{-# INLINE toAlterList #-}

-- |
liftAlter :: f a -> Alter f a
liftAlter f = Pure id `Apply` f
{-# INLINE liftAlter #-}

{- | gets rid of the @Alter@, when @f@ is already @Alternative@.

left-inverse to 'liftAlter':

@
'fellAlter' ('liftAlter' f)
'runAlter' 'id' ('Pure' id '`Apply`' f)
runAlter id (Pure id) '<*>' id f
pure id '<*>' f
f
@

-}
fellAlter :: (Alternative f) => Alter f a -> f a
fellAlter = runAlter id
{-# INLINE fellAlter #-}

{- | fold an Alter down to a value, by acting on the functor.

like 'runAlter' where @(g ~ Const b)@ but not, as 'Const' is not 'Alternative'.

warning: observes differences that runAlter doesn't; like @some x@ verses @(:) \<$> x \<*> many x@.

-}
foldAlter
 :: (forall x. f x -> b) -- ^ how to "fold down" the functor
 -> b             -- ^ a @unit@, i.e. how to interpret @'pure' _@
 -> (b -> b -> b) -- ^ @mul@, i.e. how to interpret '<*>'
 -> ([b] -> b) -- ^ @add@, i.e. how to interpret 'empty' (when the input's empty) and '<|>' (when the input's nonempty)
 -> (b -> b)   -- ^ how to interpret 'Opt'
 -> (b -> b)   -- ^ how to interpret 'Many'
 -> (b -> b)   -- ^ how to interpret 'Some'
 -> Alter f a
 -> b
foldAlter u unit mul add opt_ many_ some_ = \case
 Pure _      -> unit
 f `Apply` x -> foldAlter u unit mul add opt_ many_ some_ f `mul` u x
 f :<*>  g   -> foldAlter u unit mul add opt_ many_ some_ f `mul` foldAlter u unit mul add opt_ many_ some_ g

 Alter fs  -> add   (foldAlter u unit mul add opt_ many_ some_ <$> fs)
 Opt  _ f  -> opt_  (foldAlter u unit mul add opt_ many_ some_ f)
 Many _ f  -> many_ (foldAlter u unit mul add opt_ many_ some_ f)
 Some _ f  -> some_ (foldAlter u unit mul add opt_ many_ some_ f)

-- | 'mappend' interprets '<*>', and 'mempty' interprets @'pure' _@ (like <http://hackage.haskell.org/package/base-4.8.0.0/docs/src/Control-Applicative.html#line-84 @instance Monoid m =\> Applicative ('Const' m)@>). 'mconcat' also interprets '<|>'.
foldAlter_Monoid :: Monoid m => (forall x. f x -> m) -> Alter f a -> m
foldAlter_Monoid f = foldAlter f mempty mappend mconcat id id id

{- | interpret an Alter as some Alternative, by acting on the functor.



@
some v = some_v
where
many_v = some_v <|> pure []
some_v = (:) <$> v <*> many_v

some v = (:) <$> v <*> ...
@


-}
runAlter
 :: Alternative g
 => (forall x. f x -> g x)
 -> Alter f a
 -> g a
runAlter u = \case
 Pure a      -> pure a
 f `Apply` x -> runAlter u f <*> u x
 f :<*>    g -> runAlter u f <*> runAlter u g

 -- Alter [] -> empty
 -- Alter [f,g]  -> runAlter u f <|> runAlter u g
 Alter fs  -> asum (runAlter u `map` fs)
 Opt  f x  -> f                       <$> optional (runAlter u x)
 Many f x  -> f                       <$> many     (runAlter u x)
 Some f x  -> (f . NonEmpty.fromList) <$> some     (runAlter u x)

makeLenses ''Rule
makeLenses ''Command

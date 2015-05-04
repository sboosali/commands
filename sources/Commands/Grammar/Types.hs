{-# LANGUAGE DataKinds, DeriveFunctor, DeriveGeneric, FlexibleContexts  #-}
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures          #-}
{-# LANGUAGE NamedFieldPuns, PolyKinds, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, TypeOperators                             #-}
module Commands.Grammar.Types where
import Commands.Etc
import Control.Alternative.Free.Associated

import Control.Lens
import Data.Hashable                       (Hashable)

import GHC.Generics                        (Generic)


{- |

the type parameters are named:

* @p@ for the parser
* @r@ for the reified grammar
* @d@ for the desugarer

The type parameters are in ascending order of likeliness-to-change, for partial application.


-}
data Command p r d a = Command
 { _comGrammar  :: Grammar p r a
 , _comCompiler :: d a
 }
-- TODO  profunctor? does it matter? probably, I like thinking about it like a class: a class method to introduce, an instance method to eliminate. we might want to abstract over this, if we don't force the desugarer to be an arrow type.



{- TODO
data Command t p r d dx e a
 = Command
 { _comGrammar  :: Grammar t p r a
 , _comCompiler :: d dx a
 }

t ~ String 
exposing t might help the Interpreter,
the p must be composeable, hence not (p ~ t -> a)
but t should show up in p, anyway
should we expose the DesugaringContext?
exposing dx might help the Interpreter, as it could be unified with the current context in the state

@e@ for any user-defined environment. Induced by the root grammar. can be accessed (read-only) during parser/grammar generation.

e.g.
Command t      p            r          d               dx          e
Command String EarleyParser DNSGrammar (Op OSXActions) Application (Set Token)


-}

{- |





TODO erase RHS (at each level of Grammar) or not?
-}
data Grammar p r a = Grammar
 { _gramRule    :: Rule p r a -- ^
 , _gramGrammar :: r          -- ^
 , _gramParser  :: p a        -- ^
 }
 deriving (Functor)

-- |
--
-- as the reification @r@ and the 'LHS' are not a type constructors applied to @a@,
-- they lose no info, and can be used "as is".
-- even existentially quantified Application parsers (i.e. @Some p@) can be used
-- when the result is ignored (e.g. with '*>').
data SomeGrammar p r = forall x. SomeGrammar (Grammar p r x)

-- |
data Rule p r a = Rule
 { _ruleLHS :: !LHS
 , _ruleRHS :: RHS p r a
 }
 deriving (Functor)

-- | mono in the first, poly in the second.
bimapRule :: (LHS -> LHS) -> (RHS p r a -> RHS p r b) -> Rule p r a -> Rule p r b
bimapRule f g (Rule l r) = Rule (f l) (g r)

-- |
data LHS
 = LHS    !GUI                  -- ^ for tokens guaranteed unique by Haskell's name resolution modulo package
 | LHSInt !Int                  -- ^ for tokens guaranteed unique by safe/monadic generation
 | LHSApp !LHS [LHS]            -- ^ for reifying @app@lication of higher-order 'Rule's like @multiple@
 deriving (Show, Eq, Ord, Generic)
instance Hashable LHS

-- | unary/curried application. a specialized 'LHSApp' for convenience.
appLHS :: LHS -> LHS -> LHS
appLHS lhs = (lhs `LHSApp`) . (:[])

{- |

'RHS's must never be (directly-)recursively defined, only 'Grammar's should be
recursively defined. 'Grammar's are (like) 'RHS's tagged with unique
'LHS's. this reifies the recursion
(like searching a graph while tracking the visited set).
Some functions on 'RHS's assume non-recursive 'RHS's, just
like @('length' :: [a] -> Int)@ assumes non-infinite @[a]@.

-}
type RHS p r = Alter (Symbol p r)

data Symbol p r a = Terminal String | NonTerminal (Grammar p r a) deriving Functor

nont :: Grammar p r a -> RHS p r a
nont = liftAlter . NonTerminal

term :: String -> RHS p r a
term = liftAlter . Terminal

-- | eliminator
--
symbol :: (String -> b) -> (Grammar p r a -> b) -> Symbol p r a -> b
symbol f _ (Terminal s) = f s
symbol _ g (NonTerminal r) = g r


makeLenses ''Grammar
makeLenses ''Command
makeLenses ''Rule

gramLHS :: Lens' (Grammar p r a) LHS
gramLHS = gramRule . ruleLHS

gramRHS :: Lens' (Grammar p r a) (RHS p r a)
gramRHS = gramRule . ruleRHS

{-# LANGUAGE ConstraintKinds, DataKinds, DeriveDataTypeable                 #-}
{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE LambdaCase, NamedFieldPuns, RankNTypes, ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, TypeFamilies              #-}
{-# LANGUAGE TypeOperators                                                  #-}
-- |
module Commands.Grammar where
import           Commands.Etc
import           Commands.Grammar.Types
import           Commands.Munging
import           Control.Alternative.Free.Associated

import           Control.Lens
import           Data.Hashable

import           Control.Monad.State
import           Data.Foldable                       (asum, traverse_)
import           Data.List                           (intercalate)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe                          (fromJust)
import           Data.Monoid                         ((<>))
import           Data.Typeable                       (Typeable)
import           Language.Haskell.TH.Syntax          (Name)


vocabulary :: (Functor p) => [String] -> RHS p r String
vocabulary = asum . fmap str

str :: (Functor p) => String -> RHS p r String
str s = s <$ term s

chr :: (Functor p) => Char -> RHS p r Char
chr c = c <$ term [c]

-- | a specialization, @int = 'con'@, because integer literals are 'Num'-constrained polymorphic types.
-- in the context we will be using it, we need a concrete type for type inference.
int :: (Functor p) => Int -> RHS p r Int
int = con

con :: (Show a, Functor p) => a -> RHS p r a
con = transformedCon (intercalate " " . unCamelCase)

-- | transform a 'Show'n constructor, before making it a 'Terminal'.
transformedCon :: (Show a, Functor p) => (String -> String) -> a -> RHS p r a
transformedCon f c = c <$ (term . f . show) c

lhsOfType :: (Typeable a) => proxy a -> LHS
lhsOfType = LHS . guiOf

-- |
lhsFromName :: Name -> Possibly LHS
lhsFromName name = do
 gui <- fromName name
 return $ LHS gui

-- | safe on obviously global 'Name's, like reifying a top-level binding:
--
-- @
-- dictation = ... ('unsafeLHSFromName' \'dictation) ...
-- @
--
-- warning: partial function:
--
unsafeLHSFromName :: Name -> LHS
unsafeLHSFromName = fromJust . lhsFromName

-- | output should be unique, for "simple" inputs.
--
-- warning: partial function:
--
-- * doesn't terminate on recursive 'RHS'
-- * can't distinguish between different 'Pure's
--
-- TODO deracinate this abomination
unsafeLHSFromRHS :: RHS p r x -> LHS
unsafeLHSFromRHS rhs = LHSInt (unsafeHashRHS rhs)
 where
 unsafeHashRHS :: RHS p r x -> Int
 unsafeHashRHS (Pure _)     = hash "Pure"
 unsafeHashRHS (Many rs)    = hash "Many" `hashWithSalt` fmap unsafeHashRHS rs
 unsafeHashRHS (fs `App` x) = hash "App"  `hashWithSalt` unsafeHashRHS fs `hashWithSalt` hashSymbol x
 unsafeHashRHS (fs :<*> xs) = hash ":<*>" `hashWithSalt` unsafeHashRHS fs `hashWithSalt` unsafeHashRHS xs
 hashSymbol :: Symbol p r x -> Int
 hashSymbol = symbol hash (hash . view gramLHS)

-- | 'Identifier' for readability, 'hash'/'showHex' for uniqueness/compactness.
--
-- >>> take 10 $ showLHS (LHS (GUI (Package "package") (Module "Module.SubModule") (Identifier "identifier")))
-- "identifier"
--
-- TODO for correctness, safely associate LHSApp by checking depth
--
-- TODO for compactness, keep unique fully qualified identifier, but later render as unqualified identifier with possible compact unique suffix
showLHS :: LHS -> String
showLHS (LHS (GUI (Package "") (Module "") (Identifier occ))) = occ -- TODO compactLHS
showLHS (LHS (GUI (Package pkg) (Module mod) (Identifier occ))) = intercalate "__" [occ, mod, pkg]
 -- = occ <> "__" <> hashAlphanumeric (pkg <> "__" <> mod <> "__" <> occ)
showLHS (LHSInt i) = "i_" <> hashAlphanumeric i
showLHS (l `LHSApp` ls) = intercalate "____" (showLHS l : fmap showLHS ls)


-- | transforms a possibly-infinite structure with direct references (i.e. a recursively-defined 'Grammar') into a certainly-finite structure (i.e. a 'Map') with indirect references (i.e. its keys).
--
-- returns all a 'Grammar' 's transitive descendents (children, children's children, etc.).
--
-- even without their types, you can still extract their untyped 'LHS', count them, etc.
--
-- a parent can be its own descendent, i.e.
--
-- @Some grammar `elem` comDescendents grammar@
--
-- is @True@.
-- this means the grammar is (self- / mutually-) recursive.
--
-- Note on Naming: the "reify" means "reifying the references between rules".
--
-- this is slightly distinct from the @r@ type parameter,
-- though "inducing a reification" (i.e. a function like @induceR :: 'RHS' p r x -> r@)
-- may use this function.
reifyGrammar :: Grammar p r x -> ReifiedGrammar p r
reifyGrammar grammar = execState (reifyGrammar' grammar) Map.empty

reifyGrammar' :: Grammar p r x -> State (ReifiedGrammar p r) ()
reifyGrammar' grammar = do
 let Rule l r = grammar ^. gramRule
 visited <- gets $ Map.member l
 unless visited $ do
  modify $ Map.insert l (SomeGrammar grammar)
  reifyRHS' r

-- TODO is this a traversal or something? over the functor
reifyRHS' :: RHS p r x -> State (ReifiedGrammar p r) ()
reifyRHS' (Pure _)     = return ()
reifyRHS' (Many rs)    = traverse_ reifyRHS' rs
reifyRHS' (fs `App` x) = reifyRHS' fs >> symbol (\_ -> return ()) reifyGrammar' x
reifyRHS' (fs :<*> xs) = reifyRHS' fs >> reifyRHS' xs

type ReifiedGrammar p r = Map LHS (SomeGrammar p r)


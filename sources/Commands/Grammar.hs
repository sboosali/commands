{-# LANGUAGE ConstraintKinds, DataKinds, DeriveDataTypeable                 #-}
{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE LambdaCase, NamedFieldPuns, RankNTypes, ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, TypeFamilies              #-}
{-# LANGUAGE TypeOperators                                                  #-}
-- |
module Commands.Grammar where
import           Commands.Command.Types        ()
import           Commands.Etc
import           Commands.Grammar.Types
import           Commands.Munging
import           Control.Alternative.Free.Tree

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Char
import           Data.Foldable                 (asum, traverse_)
import           Data.Hashable
import           Data.List                     (intercalate)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromJust)
import           Data.Monoid                   ((<>))
import           Data.Typeable                 (Typeable)
import           Language.Haskell.TH.Syntax    (Name)


vocabulary :: [String] -> RHS String
vocabulary = asum . fmap str

str :: String -> RHS String
str s = s <$ liftString s

chr :: Char -> RHS Char
chr c = c <$ liftString [c]

con :: (Show a) => a -> RHS a
con c = c <$ (liftString . intercalate " " . unCamelCase . show) c

int :: Int -> RHS Int
int = con

-- |
qualifiedCon :: (Show a) => String -> a -> RHS a
qualifiedCon q c = c <$ (liftString . intercalate " " . filter (/= fmap toLower q) . unCamelCase . show) c

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
-- * doesn't distinguish between different 'Pure's
--
-- TODO deracinate this abomination
unsafeLHSFromRHS :: RHS x -> LHS
unsafeLHSFromRHS rhs = LHSInt (unsafeHashRHS rhs)
 where
 unsafeHashRHS :: RHS x -> Int
 unsafeHashRHS (Pure _)     = hash "Pure"
 unsafeHashRHS (Many rs)    = hash "Many" `hashWithSalt` fmap unsafeHashRHS rs
 unsafeHashRHS (fs `App` x) = hash "App"  `hashWithSalt` unsafeHashRHS fs `hashWithSalt` hashSymbol x
 unsafeHashRHS (fs :<*> xs) = hash ":<*>" `hashWithSalt` unsafeHashRHS fs `hashWithSalt` unsafeHashRHS xs
 hashSymbol :: Symbol x -> Int
 hashSymbol = symbol (hash . unWord) (hash . view gramLHS)

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
reifyGrammar :: Grammar x -> ReifiedGrammar
reifyGrammar grammar = execState (reifyGrammar_ grammar) Map.empty

reifyGrammar_ :: Grammar x -> State ReifiedGrammar ()
reifyGrammar_ grammar = do
 let Rule l r = grammar ^. gramRule
 visited <- gets $ Map.member l
 unless visited $ do
  modify $ Map.insert l (Some grammar)
  reifyRHS_ r

-- TODO is this a traversal or something? over the functor
reifyRHS_ :: RHS x -> State ReifiedGrammar ()
reifyRHS_ (Pure _)     = return ()
reifyRHS_ (Many rs)    = traverse_ reifyRHS_ rs
reifyRHS_ (fs `App` x) = reifyRHS_ fs >> symbol (\_ -> return ()) reifyGrammar_ x
reifyRHS_ (fs :<*> xs) = reifyRHS_ fs >> reifyRHS_ xs

type ReifiedGrammar = Map LHS (Some Grammar)

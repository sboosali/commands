{-# LANGUAGE ConstraintKinds, DataKinds, DeriveDataTypeable                 #-}
{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables                    #-}
{-# LANGUAGE StandaloneDeriving, TypeFamilies, TypeOperators                #-}
-- |
module Commands.Grammar where
import           Commands.Etc
import           Commands.Grammar.Types
import           Commands.Munging
import           Control.Alternative.Free.Tree

import           Control.Applicative
import           Control.Monad                 (unless)
import           Control.Monad.Trans.State
import           Data.Foldable                 (asum)
import           Data.Foldable                 (traverse_)
import           Data.Hashable
import           Data.List                     (intercalate)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Monoid                   ((<>))
import           Data.Proxy
import           Data.Typeable                 (Typeable)
import           Language.Haskell.TH.Syntax    (Name)
import           Numeric


infix  2 <=>
-- infixl 3 <|>
-- infixl 4 <$>
infixl 4 #
-- infixl 4 <*>
infixl 9 &

(<=>) :: Name -> RHS a -> Rule a
name <=> rs = Rule l rs
 where Just l = fromName name

-- (&) :: (Grammatical a) => RHSs (a -> b) -> a -> RHSs b
-- f & x = f <*> toR x
(&) :: Applicative f => f (a -> b) -> f a -> f b
f & x = f <*> x

-- (#) :: (Grammatical a) => (a -> b) -> a -> RHSs b
-- f # x = f <$> toR x
(#) :: Functor f => (a -> b) -> f a -> f b
f # x = f <$> x

-- class Grammatical a where
--  type R a :: *
--  toR :: a -> RHSs (R a)

-- instance Grammatical (RHSs b)    where  type R (RHSs b)    = b;  toR = id
-- instance Grammatical String      where  type R String      = b;  toR = lift . Terminal
-- -- instance Grammatical (Rule b) where  type R (Rule b) = b;  toR = lift
-- instance Grammatical (Rule b) where
--  type R (Rule b) = b
--  toR (Terminal s)      = toR s
--  toR (NonTerminal _ r) = r

project :: Rule a -> RHS a
project = lift . fromRule

terminal :: String -> RHS a
terminal = lift . fromWord . Word

terminals :: [String] -> RHS String
terminals = asum . fmap str

con :: (Show a) => a -> RHS a
con c = c <$ (terminal . intercalate " " . unCamelCase . show) c

int :: Int -> RHS Int
int = con

str :: String -> RHS String
str s = s <$ terminal s

-- | a default 'Rule' for simple ADTs.
--
-- with 'Enum' ADTs, we can get the "edit only once" property: edit the @data@ definition, then 'terminal' builds the 'Rule', and then the functions on 'Rule's build the 'Parse'rs and 'Render'ers. without any TemplateHaskell.
--
-- the 'LHS' comes from the type, not the term (avoiding TemplateHaskell). other 'Rule's can always be defined with an LHS that comes from the term, e.g. with '<=>'.
--
--
defaultRule :: forall a. (Typeable a, Enum a, Show a) => Rule a
defaultRule = Rule
 (lhsOfType (Proxy :: Proxy a))
 (asum . fmap con $ constructors)

lhsOfType :: (Typeable a) => proxy a -> LHS
lhsOfType = guiOf

-- | 'Identifier' for readability, 'hash'/'showHex' for uniqueness/compactness.
--
-- >>> take 10 $ showLHS (GUI (Package "package") (Module "Module.SubModule") (Identifier "identifier"))
-- "identifier"
--
--
-- TODO for compactness, keep unique fully qualified identifier, but later render as unqualified identifier with possible compact unique suffix
showLHS :: LHS -> String
showLHS (GUI (Package pkg) (Module mod) (Identifier occ)) = occ <> "__" <> showHex (abs . hash $ pkg <> "__" <> mod <> "__" <> occ) ""

-- | transforms a possibly-infinite structure with direct references (i.e. a recursively-defined 'Rule') into a certainly-finite structure (i.e. a 'Map') with indirect references (i.e. its keys).
--
-- Note on Naming: the "reify" means "reifying the references between rules".
reifyRule :: Rule x -> ReifiedRule
reifyRule rule = execState (reifyRule_ rule) Map.empty

reifyRule_ :: Rule x -> State ReifiedRule ()
reifyRule_ (Rule l rs) = do
 visited <- gets $ Map.member l
 unless visited $ do
  modify $ Map.insert l (Some rs)
  reifyRHS_ rs

-- TODO  is this a traversal or something?
reifyRHS_ :: RHS x -> State ReifiedRule ()
reifyRHS_ (Pure _)     = return ()
reifyRHS_ (Many rs)    = traverse_ reifyRHS_ rs
reifyRHS_ (fs `App` x) = reifyRHS_ fs >> symbol (\_ -> return ()) reifyRule_ x

reifyRHS_ (fs :<*> xs) = reifyRHS_ fs >> reifyRHS_ xs

type ReifiedRule = Map LHS (Some RHS)

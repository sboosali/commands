{-# LANGUAGE ConstraintKinds, DataKinds, DeriveDataTypeable                 #-}
{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables                    #-}
{-# LANGUAGE StandaloneDeriving, TypeFamilies, TypeOperators                #-}
-- |
module Commands.Grammar where
import Commands.Etc
import Commands.Grammar.Types
import Commands.Munging
import Control.Alternative.Free.Tree

import Control.Applicative
import Data.Foldable                 (asum)
import Data.Hashable
import Data.List                     (intercalate)
import Data.Monoid                   ((<>))
import Data.Typeable                 (Typeable)
import Numeric


terminal :: String -> RHS a
terminal = lift . fromWord . Word

alias :: [String] -> RHS String
alias = asum . fmap str

con :: (Show a) => a -> RHS a
con c = c <$ (terminal . intercalate " " . unCamelCase . show) c

int :: Int -> RHS Int
int = con

str :: String -> RHS String
str s = s <$ terminal s

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
showLHS (GUI (Package pkg) (Module mod) (Identifier occ))
 = occ <> "__" <> showHex (abs . hash $ pkg <> "__" <> mod <> "__" <> occ) ""


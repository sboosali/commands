{-# LANGUAGE ConstraintKinds, DataKinds, DeriveDataTypeable                 #-}
{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE LambdaCase, NamedFieldPuns, RankNTypes, ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving, TypeFamilies, TypeOperators                #-}
-- |
module Commands.Grammar where
import Commands.Command.Types        ()
import Commands.Etc
import Commands.Grammar.Types
import Commands.Munging
import Control.Alternative.Free.Tree

import Control.Applicative
import Data.Foldable                 (asum, foldMap)
import Data.Hashable
import Data.List                     (intercalate)
import Data.Monoid                   ((<>))
import Data.Typeable                 (Typeable)
import Language.Haskell.TH.Syntax    (Name)
import Numeric


alias :: [String] -> RHS String
alias = asum . fmap str

con :: (Show a) => a -> RHS a
con c = c <$ (liftString . intercalate " " . unCamelCase . show) c

int :: Int -> RHS Int
int = con

str :: String -> RHS String
str s = s <$ liftString s

lhsOfType :: (Typeable a) => proxy a -> LHS
lhsOfType = LHS . guiOf

-- |
--
-- warning: partial function.
unsafeLHSFromName :: Name -> LHS
unsafeLHSFromName name = LHS gui
 where Just gui = fromName name


-- | 'Identifier' for readability, 'hash'/'showHex' for uniqueness/compactness.
--
-- >>> take 10 $ showLHS (LHS (GUI (Package "package") (Module "Module.SubModule") (Identifier "identifier")))
-- "identifier"
--
-- TODO for correctness, safely associate LHSApp by checking depth
--
-- TODO for compactness, keep unique fully qualified identifier, but later render as unqualified identifier with possible compact unique suffix
showLHS :: LHS -> String
showLHS (LHS (GUI (Package pkg) (Module mod) (Identifier occ)))
 = occ <> "__" <> showHex (abs . hash $ pkg <> "__" <> mod <> "__" <> occ) ""
showLHS (l `LHSApp` ls) = intercalate "___" (showLHS l : fmap showLHS ls)

-- |
--
-- removes duplicates with 'nubBy', which takes time quadratic in the length of the list.
--
-- includes the input "parent" in the output, if it's its own "child".
getChildren :: Rule x -> [Some Command]
getChildren (Rule l r)
 = filter ((l /=) . theLHS)
 $ getChildren_ r
 where
 theLHS = (\(Some (Command{_lhs})) -> _lhs)

-- TODO is this a traversal or something? over the functor
getChildren_ :: RHS x -> [Some Command]
getChildren_ (Pure _)     = []
getChildren_ (Many rs)    = getChildren_ `foldMap` rs
getChildren_ (fs `App` x) = getChildren_ fs <> symbol (const []) ((:[]) . Some) x
getChildren_ (fs :<*> xs) = getChildren_ fs <> getChildren_ xs

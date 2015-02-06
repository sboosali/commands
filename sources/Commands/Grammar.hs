{-# LANGUAGE ConstraintKinds, DataKinds, DeriveDataTypeable                 #-}
{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, StandaloneDeriving            #-}
{-# LANGUAGE TypeFamilies, TypeOperators                                    #-}
-- |
module Commands.Grammar where
-- import Commands.Etc
-- import Commands.Grammar.Types
-- import Language.Haskell.TH.Syntax (Name(..),NameFlavour(NameG),OccName(..),PkgName(..),ModName(..))


-- infixr 9 &
-- infix  4 #
-- infix  3 #=
-- infixl 2 #|


-- (<=>) :: Name -> RHS a -> Grammar a
-- name <=> r =

-- (#) ::
-- (#) = (<$>)

-- (&) :: (Grammatical a x) => a -> Grammar (a -> b) -> Grammar b
-- x & y = toGrammar x <*> y

-- class    Grammatical a x | a -> x           where  toGrammar :: a -> Grammar x
-- instance Grammatical String          String where  toGrammar = Terminal
-- instance Grammatical (Grammar x) x          where  toGrammar = id

-- fromName :: Name -> LHS
-- fromName (Name (OccName occ) (NameG _ (PkgName pkg) (ModName mod))) = LHS pkg mod occ

-- terminal :: String -> Grammar String
-- terminal s = (dragon s) (SensitiveParser $ \_ -> word s)

-- con :: (Show a) => a -> Grammar a
-- con c = c <$ (terminal . intercalate " " . unCamelCase . show) c

-- int :: Int -> Grammar Int
-- int = con

-- twig :: (Enum a, Show a) => Grammar a
-- twig = asum . map con $ constructors


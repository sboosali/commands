{-# LANGUAGE ConstraintKinds, DataKinds, DeriveDataTypeable                 #-}
{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, StandaloneDeriving            #-}
{-# LANGUAGE TypeFamilies, TypeOperators                                    #-}
-- |
module Commands.Grammar where
import Commands.Etc
import Commands.Grammar.Types
import Language.Haskell.TH.Syntax (Name)


infix  2 <=>
-- infixl 3 <|>
-- infixl 4 <*>
-- infixr 9 &

(<=>) :: Name -> RHSs a -> Grammar a
name <=> rs = NonTerminal l rs
 where Just l = fromName name

-- (&) :: (Grammatical a x) => a -> Grammar (a -> b) -> Grammar b
-- x & y = toGrammar x <*> y

-- class    Grammatical a x | a -> x  where  toGrammar :: a -> RHSs x
-- instance Grammatical String      x where  toGrammar = Terminal
-- instance Grammatical (Grammar x) x where  toGrammar =
-- instance Grammatical (RHSs x)    x where  toGrammar = id

-- terminal :: String -> Grammar a
-- terminal s = Terminal s

-- terminals = map terminal

-- con :: (Show a) => a -> Grammar a
-- con c = c <$ (terminal . intercalate " " . unCamelCase . show) c

-- int :: Int -> Grammar Int
-- int = con

-- str :: String -> Grammar String
-- str s = s <$ terminal s

-- twig :: (Enum a, Show a) => Grammar a
-- twig = foldr (<|>) empty . map con $ constructors


{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable, DeriveFunctor    #-}
{-# LANGUAGE ExtendedDefaultRules, LambdaCase, OverloadedStrings      #-}
{-# LANGUAGE PartialTypeSignatures, PatternSynonyms, PostfixOperators #-}
{-# LANGUAGE RankNTypes, TupleSections, StandaloneDeriving                                             #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-imports -fno-warn-type-defaults -fno-warn-partial-type-signatures #-}
module Commands.Plugins.Example.Emacs where
import           Commands.RHS.Types
import  qualified Data.HRefCache.Internal as HRefCache

import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty  as NonEmpty
import qualified Text.Earley.Internal as E
import qualified Text.Earley.Grammar as E
import qualified Text.Earley as E

import           Control.Applicative
import Data.STRef
import Control.Monad.ST
import           Control.Arrow ((>>>))
import Data.Functor.Product


(<=>) n r = NonTerminal (RHSName n r)
infix 2 <=>

edits :: RHS (RHSName String) String [] (NonEmpty Edit)
-- edits :: RHS (RHSName String) String _ (NonEmpty Edit)
-- edits :: Functor f => RHS (RHSName String) String f (NonEmpty Edit)
edits = "edits" <=> (edit-+)

data Edit = Edit Action Slice Region deriving (Show,Eq,Ord)
edit = "edit" <=> empty
 <|> Edit Cut Forwards Line <$ "kill"
 <|> Edit <$> action            <*> (slice-?-Whole) <*> (region-?-That)
 <|> Edit <$> (action-?-Select) <*> (slice-?-Whole) <*> region

data Action
 = Select                       -- read-only.
 | Copy                         -- read-only.
 | Cut                          -- read/write.
 | Delete                       -- read/write.
 | Transpose                    -- read/write.
 | Google                       -- read-only.
 deriving (Show,Eq,Ord)
action = "action" <=> empty
 <|> Select      <$ "sell"
 <|> Copy        <$ "cop"
 <|> Cut         <$ "kill"
 <|> Delete      <$ "del"
 <|> Transpose   <$ "trans"
 <|> Google      <$ "google"

data Slice = Whole | Backwards | Forwards  deriving (Show,Eq,Ord,Enum)
slice = "slice"
 <=> Whole     <$ "whole"
 <|> Backwards <$ "back"
 <|> Forwards  <$ "for"

data Region
 = That
 | Character
 | Word_                        -- ^ e.g. @"camelCase"@, @"lisp-case"@, @"snake_case"@ are all two 'Word_'s
 | Token                        -- ^ e.g. @"camelCase"@, @"lisp-case"@, @"snake_case"@ are all one 'Token's
 | Group                        -- ^ 'Bracket's delimit 'Group's (e.g. @"(...)"@ or @"<...>"@ or @"[|...|]"@)
 | Line
 | Rectangle
 | Block
 | Page
 | Screen
 | Everything
 | Definition
 | Function_
 | Reference
 | Structure
 deriving (Show,Eq,Ord,Enum)
region = "region"
 <=> That       <$ "that"
 <|> Character  <$ "char"
 <|> Word_      <$ "word"
 <|> Token      <$ "toke"
 <|> Group      <$ "group"
 <|> Line       <$ "line"
 <|> Rectangle  <$ "wreck"
 <|> Block      <$ "block"
 <|> Page       <$ "page"
 <|> Screen     <$ "screen"
 <|> Everything <$ "all"
 <|> Definition <$ "def"
 <|> Function_  <$ "fun"
 <|> Reference  <$ "ref"
 <|> Structure  <$ "struct"

renameRHSST
 :: (forall x. RHS n t f x -> n t f x -> ST z (n' t f x))
 -> ST z (RHS n t f a -> ST z (RHS n' t f a))
renameRHSST = undefined
-- renameRHSST u = do
--  c <- HRefCache.newCache
--  return$ renameRHS$ \t x -> do
--   k <- HRefCache.forceStableName t
--   readSTRef c >>= (HRefCache.lookupRef k >>> traverse readSTRef) >>= \case
--    Just y  -> return y
--    Nothing -> do
--     y <- u t x
--     v <- newSTRef y
--     _ <- modifySTRef' c ((,()) . HRefCache.insertRef k v)
--     return y

data RHSName n t f a = RHSName !n (RHS (RHSName n) t f a)
 deriving (Functor)

data EarleyName z r n t f a = EarleyName
 !n
 !(STRef z          (Maybe [a]))
 !(STRef z (STRef z [r]))
 (RHS (EarleyName z r n) t f a)
deriving instance Functor (EarleyName z r n t f)

deriveEarley :: RHS (RHSName n) t f a -> ST z (RHS (EarleyName z r n) t f a) 
deriveEarley = renameRHSST $ \(RHSName n r) -> do
 conts <- newSTRef =<< newSTRef []
 null  <- newSTRef Nothing
 return$ EarleyName n conts null r

induceEarley :: RHS (EarleyName z (E.Cont z r n t a r) n) t (Product (E.Prod z n t) _) a -> E.Prod z n t a
induceEarley = runRHS fromN fromT fromF
 where
 fromN (EarleyName n null conts r) = E.NonTerminal (E.Rule (induceEarley r) null conts) (E.Pure id) E.<?> n
 fromT = E.symbol
 fromF (Pair p _) = p

{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable, DeriveFunctor    #-}
{-# LANGUAGE ExtendedDefaultRules, LambdaCase, OverloadedStrings      #-}
{-# LANGUAGE PartialTypeSignatures, PatternSynonyms, PostfixOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-imports -fno-warn-type-defaults -fno-warn-partial-type-signatures #-}
module Commands.Plugins.Example.Emacs where
import           Commands.RHS.Types

import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty  as NonEmpty

import           Control.Applicative


data RHSName t f a = RHSName String (RHS RHSName t f a) deriving (Functor)

(<=>) n r = NonTerminal (RHSName n r)
infix 2 <=>

edits :: RHS RHSName String [] (NonEmpty Edit)
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


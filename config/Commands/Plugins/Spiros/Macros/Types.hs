{-# LANGUAGE OverloadedStrings, RankNTypes, TypeOperators, LambdaCase, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, GADTs, TypeFamilies, ConstraintKinds, DataKinds #-}

module Commands.Plugins.Spiros.Macros.Types where
import Commands.Plugins.Spiros.Extra
import Commands.Plugins.Spiros.Apply

import Commands.RHS.Types 
import Commands.Mixins.DNS13OSX9
import           Commands.Backends.OSX

import Data.Function (on)
import           GHC.Exts                        (IsString)
import Language.Haskell.TH.Syntax (Name, mkName) 
import Control.Arrow (second) 


newtype Macro = Macro (Apply IsMacroArgument CWorkflow_)

instance Eq  Macro where (==)    = (==)    `on` getMacroName -- TODO lawless 
instance Ord Macro where compare = compare `on` getMacroName -- TODO lawless 

instance Show     Macro where show = showMacro -- TODO lawless 
instance Rankable Macro where rank = rankMacro

{-| an argument to a Macro must satisfy these constraints 

-}
type IsMacroArgument = '[Rankable, Show] 

showMacro :: Macro -> String
showMacro = show . getMacroName
-- showMacro

rankMacro :: Macro -> Int
rankMacro = unMacro >>> \case
 A0 _ _         -> defaultRank 
 A1 _ _ a       -> 10 + sum [rankAtLeast 1 a]
 A2 _ _ a b     -> 20 + sum [rankAtLeast 1 a, rankAtLeast 1 b]           -- omg the hackiness
 A3 _ _ a b c   -> 30 + sum [rankAtLeast 1 a, rankAtLeast 1 b, rankAtLeast 1 c]
 A4 _ _ a b c d -> 40 + sum [rankAtLeast 1 a, rankAtLeast 1 b, rankAtLeast 1 c, rankAtLeast 1 d]

-- | destructor 
unMacro :: Macro -> (Apply IsMacroArgument CWorkflow_)
unMacro (Macro f) = f

{-| since macros are "function-like", we give them an "identity" for equality/debugging. 

-}
getMacroName :: Macro -> Name 
getMacroName = getApplyName . unMacro

aliasMacro :: (IsString t, Show t, Functor'RHS n t f) => (a -> CWorkflow_) -> [(String, a)] -> RHS n t f Macro 
aliasMacro f = vocabMacro . fmap (second f) 

{-| a specialized vocabulary where the macro name comes from the dict key.  

-}
vocabMacro :: (IsString t, Show t, Functor'RHS n t f) => [(String, CWorkflow_)] -> RHS n t f Macro 
vocabMacro = vocab . fmap makeMacro 

runMacro :: Macro -> CWorkflow_
runMacro = runApply . unMacro

makeMacro :: (String, CWorkflow_) -> (String, Macro)
makeMacro (name,workflow) = (name, Macro (A0 (mkName name) workflow))


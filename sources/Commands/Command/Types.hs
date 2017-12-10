{-# LANGUAGE TemplateHaskell #-}
module Commands.Command.Types where
import Commands.RHS.Types (RHS) 

import Data.List.NonEmpty (NonEmpty) 
import           Control.Lens


data Command n t f c b a = Command
 { _cRHS     :: RHS n t f a         -- ^ the root of a grammar
 , _cBest    :: NonEmpty a -> a    -- ^ for disambiguating multiple parse results
 , _cDesugar :: c -> a -> b    -- ^ "desugar" the parse result into "bytecode" actions
 }

makeLenses ''Command


{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Main where 
-- import Commands.Playground

import           System.Environment             (getArgs)
import Control.Monad

import Prelude

main = mainWith =<< getArgs

mainWith = \case
 _ -> return() 


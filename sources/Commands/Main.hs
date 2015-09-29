{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Main where 

import Data.Canonical 

import           System.Environment             (getArgs)


main = mainWith =<< getArgs

mainWith = \case
 _ -> mainCanonical 


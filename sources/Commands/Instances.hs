{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Commands.Instances where

import Control.Monad.Catch
import Text.Parsec         (ParseError)

import Data.Typeable


deriving instance Typeable  ParseError
instance          Exception ParseError

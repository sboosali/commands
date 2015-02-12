{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Commands.Instances where

import Control.Monad.Catch (Exception)
import Data.Typeable       (Typeable)
import Text.Parsec         (ParseError)


deriving instance Typeable  ParseError
instance          Exception ParseError


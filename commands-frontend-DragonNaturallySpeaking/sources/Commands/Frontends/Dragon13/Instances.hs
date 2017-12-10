{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | orphan instances
module Commands.Frontends.Dragon13.Instances where

import           Control.Monad.Catch               (Exception)
import           Data.Typeable                     (Typeable)
import qualified Language.Python.Common.ParseError as Python

deriving instance Typeable  Python.ParseError
instance          Exception Python.ParseError

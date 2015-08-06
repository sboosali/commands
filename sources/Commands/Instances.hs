{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | orphan instances
module Commands.Instances where

import           Control.Monad.Catch               (Exception)
import           Data.Typeable                     (Typeable)
import qualified Language.Python.Common.ParseError as Python
import qualified Text.Earley                       as E


-- | the superclass constraints on 'Exception'. uses @ConstraintKinds@.
--
-- when you see
--
-- @
-- myHandler :: (Exceptional a) => a -> ...
-- @
--
-- it implies the provenance of those constraints is something like:
--
-- @
-- data MyError a = ...
-- instance ('Typeable' a, 'Show' a) => 'Exception' (MyError a)
-- @
--
-- this helps with reading/refactoring heavily-constrained types.
type Exceptional a = (Typeable a, Show a)

deriving instance                           Typeable  (E.Report e ts)
instance (Exceptional e, Exceptional ts) => Exception (E.Report e ts)

deriving instance Typeable  Python.ParseError
instance          Exception Python.ParseError

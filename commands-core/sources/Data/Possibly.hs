{-# LANGUAGE RankNTypes #-}
module Data.Possibly
 ( module Data.Possibly
 , module Control.Monad.Catch
 ) where

import           Control.Monad.Catch          (MonadThrow(..))

import Prelude

{-| generalized 'Maybe':

>>> (return "actually" :: Possibly String) :: Maybe String
Just "actually"

>>> (return "actually" :: Possibly String) :: [String]
["actually"]

>>> import Control.Exception
>>> (return "actually" :: Possibly String) :: Either SomeException String
Right "actually"

-}
type Possibly a = forall m. (MonadThrow m) => m a

-- ================================================================ --

failed :: String -> Possibly a
failed = throwM . userError


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Address where

import Servant.Common.BaseUrl (BaseUrl(..), Scheme(..))
--TODO import           Formatting                   (format, shown, string, (%))
import           Data.Text.Lazy               (Text)
import           Data.Text.Lazy as T
import           Control.Lens                 (makeLenses,makePrisms)

import           GHC.Generics                 (Generic)
import Data.Data (Data)
-- import Data.Function ((&))
import Data.Monoid
import GHC.Exts (IsString(..))


data Address = Address
 { _host :: Host
 , _port :: Port
 } deriving (Show,Read,Eq,Ord,Data,Generic)

newtype Host = Host String deriving (Show,Read,Eq,Ord,Data,Generic)
newtype Port = Port Int    deriving (Show,Read,Eq,Ord,Data,Generic)
--TODO Word64
--NOTE no Num

instance IsString Host where fromString = Host

-- ================================================================ --

localhost :: Host
localhost = "localhost"

-- | >>> displayAddress$ Address (Host "localhost") (Port 8000)
-- "http://localhost:8000"
displayAddress :: Address -> Text
displayAddress (Address (Host h) (Port p)) = T.pack url
 where url = "http://"<>h<>":"<>(show p)

address2baseurl :: Address -> BaseUrl
address2baseurl (Address (Host h) (Port p)) = BaseUrl Http h p ""

-- ================================================================ --

makeLenses ''Address
makePrisms ''Host
makePrisms ''Port

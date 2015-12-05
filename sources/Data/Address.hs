{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Address where 

import Servant.Common.BaseUrl (BaseUrl(..), Scheme(..)) 
import           Formatting                   (format, shown, string, (%))
import           Data.Text.Lazy               (Text)
import           Control.Lens                 (makeLenses,makePrisms)

import           GHC.Generics                 (Generic)
import Data.Data (Data) 


data Address = Address
 { _host :: Host
 , _port :: Port
 } deriving (Show,Read,Eq,Ord,Data,Generic)

newtype Host = Host String deriving (Show,Read,Eq,Ord,Data,Generic)
newtype Port = Port Int    deriving (Show,Read,Eq,Ord,Data,Generic)

-- ================================================================ --

localhost :: Host 
localhost = Host "localhost" 

-- | >>> displayAddress$ Address (Host "localhost") (Port 8000)
-- "http://localhost:8000"
displayAddress :: Address -> Text
displayAddress (Address (Host h) (Port p)) = format ("http://"%string%":"%shown) h p

address2baseurl :: Address -> BaseUrl
address2baseurl (Address (Host h) (Port p)) = BaseUrl Http h p 

-- ================================================================ --

makeLenses ''Address
makePrisms ''Host
makePrisms ''Port

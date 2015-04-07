{-# LANGUAGE DeriveGeneric, LambdaCase #-}
module Commands.Servers.Servant.Types where

import Data.Aeson
import Data.Aeson.Types

import Data.Map         (Map)
import GHC.Generics     (Generic)


{- |



-}
newtype Recognition = Recognition [String]  deriving (Show,Eq,Ord,Generic)
instance FromJSON Recognition

{- |

>>> import qualified Data.ByteString.Lazy.Char8 as B
>>> (putStr . B.unpack . 'encode') (DGNUpdate Nothing Nothing Nothing :: DGNUpdate String)
{"dgnSerializedExports":null,"dgnSerializedRules":null,"dgnSerializedLists":null}
>>> (putStr . B.unpack . 'encode') $ DGNUpdate (Just "rule") (Just ["export_one", "export_two"]) (Just $ Map.fromList [("list_one", ["a","b","c"]),("list_two", ["a","b","c"])])
{"dgnSerializedExports":["export_one","export_two"],"dgnSerializedRules":"rule","dgnSerializedLists":{"list_two":["a","b","c"],"list_one":["a","b","c"]}}


should be a sum type, but it must be a silly product type of Maybe\'s:
 the server can't send requests to the client (due to the single-threaded client, see TODO);
 we don't want to waste 100 ms in multiple requests after every recognition.

parameterized over your choice of string-like type (or any type at all).

-}
data DGNUpdate a = DGNUpdate
 { _dgnSerializedRules   :: Maybe a -- ^ serializes to a JSON String 'Value' or 'Null'
 , _dgnSerializedExports :: Maybe [a] -- ^ serializes to a JSON 'Array' or 'Null'
 , _dgnSerializedLists   :: Maybe (Map String [a]) -- ^ serializes to a JSON 'Object' or 'Null'
 } deriving (Show,Eq,Ord,Generic)

instance ToJSON a => ToJSON (DGNUpdate a) where
 toJSON = genericToJSON defaultOptions{ fieldLabelModifier = stripUnderscorePrefix }
  where stripUnderscorePrefix = (\case ('_':xs) -> xs; xs -> xs)


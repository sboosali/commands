{-# LANGUAGE DeriveGeneric, LambdaCase #-}
module Commands.Servers.Servant.Types where
import Commands.Backends.OSX.Types     (Actions_, Application,
                                        ApplicationDesugarer)
import Commands.Mixins.DNS13OSX9.Types (C)

import Data.Aeson
-- import Data.Aeson.Types

-- import Data.Map         (Map)
import GHC.Generics                    (Generic)


{- | the Commands Model.

State that configures the server's handlers, and may be updated by clients.

'_modContext' can be updated by clients and used by the '_modCommand', concurrently.

-}
data CmdModel z a = CmdModel
 { _modCommand :: C z ApplicationDesugarer Actions_ a
 , _modDefault :: String -> a   -- ^ what a failed parse should default to
  -- TODO (should move _modDefault into the parser, to make it total? But then we can't report errors unless the Either was desugar into an Action error, with ThrowA or something)
 , _modContext :: Application   -- ^ the current context (e.g. the current application).
 -- , _mod ::
 }

{- |



-}
newtype DGNRecognition = DGNRecognition [String]  deriving (Show,Eq,Ord,Generic)
instance FromJSON DGNRecognition

{- |

>>> import qualified Data.ByteString.Lazy.Char8 as B
>>> (putStr . B.unpack . 'encode') (DGNUpdate Nothing Nothing Nothing :: DGNUpdate String)
{"dgnUpdateExports":null,"dgnUpdateRules":null,"dgnUpdateLists":null}
>>> (putStr . B.unpack . 'encode') $ DGNUpdate (Just "rule") (Just ["export_one", "export_two"]) (Just $ Map.fromList [("list_one", ["a","b","c"]),("list_two", ["a","b","c"])])
{"dgnUpdateExports":["export_one","export_two"],"dgnUpdateRules":"rule","dgnUpdateLists":{"list_two":["a","b","c"],"list_one":["a","b","c"]}}


should be a sum type, but it must be a silly product type of Maybe\'s:
 the server can't send requests to the client (due to the single-threaded client, see TODO);
 we don't want to waste 100 ms in multiple requests after every recognition.

parameterized over your choice of string-like type (or any type at all).

-}
-- data DGNUpdate a = DGNUpdate
--  { _dgnUpdateRules   :: Maybe a -- ^ serializes to a JSON String 'Value' or 'Null'
--  , _dgnUpdateExports :: Maybe [a] -- ^ serializes to a JSON 'Array' or 'Null'
--  , _dgnUpdateLists   :: Maybe (Map String [a]) -- ^ serializes to a JSON 'Object' or 'Null'
--  } deriving (Show,Eq,Ord,Generic)

-- instance ToJSON a => ToJSON (DGNUpdate a) where
--  toJSON = genericToJSON defaultOptions{ fieldLabelModifier = stripUnderscorePrefix }
--   where stripUnderscorePrefix = (\case ('_':xs) -> xs; xs -> xs)


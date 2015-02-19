module Commands.Test.Types where
import Data.Text.Lazy  (Text)
import Test.QuickCheck


newtype TinyText = TinyText { unTinyText :: Text } deriving (Show, Eq, Ord)

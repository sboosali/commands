module Commands.Test.Types where
import Test.QuickCheck
import           Data.Text.Lazy      (Text)


newtype TinyText = TinyText { unTinyText :: Text } deriving (Show, Eq, Ord)

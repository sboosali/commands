module Commands.Test.Etc where
import Test.QuickCheck


resized :: (Arbitrary a) => Int -> Gen a -> Gen a
resized k g = sized (\n -> resize (n `div` k) g)

{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Commands.Etc where
import Commands.Instances  ()
import Control.Monad.Catch (MonadThrow)


-- | generalized 'Maybe':
--
-- >>> (return "actually" :: Possibly String) :: Maybe String
-- Just "actually"
--
-- >>> (return "actually" :: Possibly String) :: [String]
-- ["actually"]
--
-- >>> import Control.Exception
-- >>> (return "actually" :: Possibly String) :: Either SomeException String
-- Right "actually"
--
--
type Possibly a = (MonadThrow m) => m a

-- | existentially-quantify any unary type-constructor
--
--
data Some f = forall x. Some (f x)

-- | The constructors of a (zero-based) Enum.
--
-- >>> constructors :: [Bool]
-- [False,True]
--
constructors :: (Enum a) => [a]
constructors = enumFrom (toEnum 0)

newtype Package    = Package    String deriving (Show, Eq, Ord)
newtype Module     = Module     String deriving (Show, Eq, Ord)
newtype Identifier = Identifier String deriving (Show, Eq, Ord)


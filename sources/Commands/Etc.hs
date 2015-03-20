{-# LANGUAGE DeriveGeneric, ExistentialQuantification, FlexibleContexts #-}
{-# LANGUAGE RankNTypes                                                 #-}
-- {-# LANGUAGE GADTs, PolyKinds, KindSignatures #-}
module Commands.Etc where
import           Commands.Instances           ()

import           Control.Applicative
import           Control.Monad.Catch          (MonadThrow, throwM)
import           Control.Monad.Reader         (ReaderT, local)
import           Data.Hashable
import           Data.Monoid                  ((<>))
import           Data.Text.Lazy               (Text)
import           Data.Typeable                (Typeable, tyConModule, tyConName,
                                               tyConPackage, typeRep,
                                               typeRepTyCon)
import qualified Debug.Trace
import           GHC.Generics                 (Generic)
import           Language.Haskell.TH.Syntax   (ModName (ModName), Name (..),
                                               NameFlavour (NameG),
                                               OccName (OccName),
                                               PkgName (PkgName))
import           Numeric
import           Text.PrettyPrint.Leijen.Text (Doc, displayT, renderPretty)
-- import Data.Traversable
-- import Data.Foldable


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

failed :: String -> Possibly a
failed = throwM . userError


-- | easily define smart constructors, whose error message has a
-- fully-qualified name for debugging. if you rename the module, the
-- error message changes automatically. and if you rename the
-- identifier, you will get a compile time error from Template Haskell
-- if you don't update the error message
-- (unless another name is captured).
--
-- e.g.
--
-- @
-- natural :: Integer -> Possibly Natural
-- natural i
--  | i >= 0    = return $ Natural i
--  | otherwise = failure 'natural
-- @
--
failure :: Name -> Possibly a
failure = throwM . userError . showName

showName :: Name -> String
showName = either show showGUI . fromName

-- | only 'NameG' is global, i.e. is unique modulo package and module.
fromName :: Name -> Possibly GUI
fromName (Name (OccName occ) (NameG _ (PkgName pkg) (ModName mod))) = return $ GUI (Package pkg) (Module mod) (Identifier occ)
fromName (Name (OccName occ) _) = failed occ

-- | >>> showGUI (GUI (Package "package") (Module "Module.SubModule") (Identifier "identifier"))
-- "package-Module.SubModule.identifier"
showGUI :: GUI -> String
showGUI (GUI (Package pkg) (Module mod) (Identifier occ)) = pkg <> "-" <> mod <> "." <> occ

-- | The constructors of a (zero-based) Enum.
--
-- >>> constructors :: [Bool]
-- [False,True]
--
-- (Bounded Constraint elided for convenience; doesn't terminate on un@Bounded@ @Enum@erations)
--
constructors :: (Enum a) => [a]
constructors = enumFrom (toEnum 0)

-- | The first constructor of a (zero-based) Enum.
--
-- >>> enumDefault :: Bool
-- False
--
-- (Bounded Constraint elided for convenience; doesn't terminate on un@Bounded@ @Enum@erations)
--
enumDefault :: (Enum a) => a
enumDefault = toEnum 0

newtype Package    = Package    String deriving (Show, Eq, Ord, Generic); instance Hashable Package
newtype Module     = Module     String deriving (Show, Eq, Ord, Generic); instance Hashable Module
newtype Identifier = Identifier String deriving (Show, Eq, Ord, Generic); instance Hashable Identifier

-- | could have fourth field: @Version@.
data GUI = GUI !Package !Module !Identifier deriving (Show, Eq, Ord, Generic)
instance Hashable GUI

display :: Doc -> Text
display = displayT . renderPretty 1.0 80

-- | logical implication as Boolean propositions. makes reading validators easier. read @p --> q@ it as "p implies q".
(-->) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p --> q = ((||) <$> (not . p) <*> q)

trace :: String -> a -> a
trace = Debug.Trace.trace

traced :: (Show a) => a -> a
traced = Debug.Trace.traceShowId

tracing :: (Show a, Monad m) => a -> m ()
tracing = Debug.Trace.traceShowM

with :: Monad m => r -> ReaderT r m a -> ReaderT r m a
with = local . const

-- | the globally unique identifier of a type: @(pkg,
-- <https://www.haskell.org/onlinereport/lexemes.html modid>,
-- <https://www.haskell.org/onlinereport/lexemes.html tycon>)@
--
--
guiOf :: (Typeable a) => proxy a -> GUI
guiOf
 = (\t -> GUI (Package $ tyConPackage t) (Module $ tyConModule t) (Identifier $ tyConName t))
 . typeRepTyCon
 . typeRep

hashAlphanumeric :: (Hashable a) => a -> String
hashAlphanumeric = flip showHex "" . abs . hash

-- | existentially-quantify any unary type-constructor
--
-- >>> :t Some Nothing
-- Some Nothing :: Some Maybe
--
-- >>> case Some [] of Some xs -> length xs
-- 0
--
data Some f = forall x. Some (f x)

{- | existentially-quantify the first type-argument of any binary type-constructor

>>> :t Some2 (Right True)
Some2 (Right True) :: Some2 Either Bool

>>> :{
case Some2 (Right True) of
 Some2 (Left {}) -> False
 Some2 (Right b) -> b
:}
True

>>> :t Some (Some2 (Right ()))
Some (Some2 (Right ())) :: Some (Some2 Either)

>>> :{
case Some (Some2 (Right ())) of
 Some (Some2 (Left  {})) -> "Left"
 Some (Some2 (Right {})) -> "Right"
:}
"Right"

-}
data Some2 f y = forall x. Some2 (f x y)


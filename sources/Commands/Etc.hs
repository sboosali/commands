{-# LANGUAGE DeriveGeneric, ExistentialQuantification, FlexibleContexts #-}
{-# LANGUAGE RankNTypes, TemplateHaskell                                #-}
module Commands.Etc where
import           Commands.Instances           ()

import           Control.Lens                 (Lens', Prism', lens, prism')
import           Control.Monad.Catch          (MonadThrow, throwM)
import           Control.Monad.Reader         (ReaderT, local)
import           Data.Bifoldable              (Bifoldable, bifoldMap)
import           Data.Bifunctor               (first)
import           Data.Either.Validation       (Validation, eitherToValidation)
import           Data.Hashable
import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.Text.Lazy               (Text)
import           Numeric
import           Text.PrettyPrint.Leijen.Text (Doc, displayT, renderPretty)

import           Control.Applicative
import           Control.Exception            (Exception (..), Handler,
                                               SomeException (..), catches)
import           Data.List                    (nub)
import           Data.Monoid                  ((<>))
import           Data.Typeable                (Typeable, tyConModule, tyConName,
                                               tyConPackage, typeRep,
                                               typeRepTyCon)
import qualified Debug.Trace
import           GHC.Generics                 (Generic)
import           Language.Haskell.TH.Syntax   (ModName (ModName), Name (..),
                                               NameFlavour (NameG),
                                               OccName (OccName),
                                               PkgName (PkgName))


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


nonemptyHead :: Lens' (NonEmpty a) a
nonemptyHead = lens
 (\(x :| _)    -> x)
 (\(_ :| xs) x -> x :| xs)

nonemptyTail :: Lens' (NonEmpty a) [a]
nonemptyTail = lens
 (\(_ :| xs)   -> xs)
 (\(x :| _) xs -> x :| xs)


-- | @Either@ is a @Monad@: it short-circuits. 'Validation' is an @Applicative@, but not a @Monad@: under @traverse@ (or @bitraverse@), it runs the validation (@:: a -> f b@) on every field (@:: a@) in the traversable (@:: t a@), monoidally appending together all errors, not just the first.
eitherToValidations :: Either e a -> Validation [e] a
eitherToValidations = eitherToValidation . first (:[])

-- | a 'bifoldMap' on the left, removing duplicates.
--
--
--
--
getLefts :: (Eq n, Bifoldable p) => p n t -> [n]
getLefts = nub . bifoldMap (:[]) (const [])

-- | a 'bifoldMap' on the right, removing duplicates.
--
--
getRights :: (Eq t, Bifoldable p) => p n t -> [t]
getRights = nub . bifoldMap (const []) (:[])

-- | helper function to write manual Show instances.
 -- e.g. for existentially quantified types.
showsPrecNewtype :: (Show a) => Int -> String -> a -> ShowS
showsPrecNewtype depth name value = showParen
 (depth > 10)
 (showString (name <> " ") . showsPrec (10+1) value)

-- | see <https://hackage.haskell.org/package/lens-4.7/docs/Control-Exception-Lens.html#g:6 Control.Exception.Lens>
prismException :: (Exception e) => Prism' SomeException e
prismException = prism' SomeException fromException

-- | @handles = flip 'catches'@
handles :: [Handler a] -> IO a -> IO a
handles = flip catches


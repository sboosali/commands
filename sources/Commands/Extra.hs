{-# LANGUAGE AutoDeriveTypeable, ConstraintKinds, DataKinds, DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric, ExistentialQuantification, FlexibleContexts     #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, RankNTypes, RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell, TypeOperators                                 #-}
-- | assorted functionality, imported by most modules in this package.  
module Commands.Extra
 ( module Commands.Extra
 , module Commands.Instances
 , module Data.Data
 , module Data.HTypes 
 , module GHC.Generics
 , (>>>)
 , traverse_
 ) where
import           Commands.Instances
import Data.HTypes(Exists(..), exists) 

import           Control.Lens                 (Lens', Prism', lens, makeLenses,
                                               makePrisms, prism')
import           Control.Monad.Catch          (MonadThrow, throwM)
import           Control.Monad.Reader         (ReaderT, local)
import           Data.Bifoldable              (Bifoldable, bifoldMap)
import           Data.Bifunctor               (first)
import           Data.Either.Validation       (Validation, eitherToValidation)
import           Data.Hashable
import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.Text.Lazy               (Text)
import           Formatting                   (format, shown, string, (%))
import           Numeric
import           Text.PrettyPrint.Leijen.Text (Doc, displayT, renderPretty)
import Servant.Common.BaseUrl (BaseUrl(..), Scheme(..)) 

-- TODO import Data.Functor.Classes
import           Control.Applicative
import           Control.Arrow                ((>>>))
import           Control.Exception            (Exception (..), Handler,
                                               SomeException (..), catches)
import           Data.Graph
import           Data.List                    (nub)
import           Data.Maybe
import           Data.Monoid                  ((<>))
import           Data.Ord
import           Data.Typeable                (Typeable, tyConModule, tyConName,
                                               tyConPackage, typeRep,
                                               typeRepTyCon)
import qualified Debug.Trace
import           GHC.Generics                 (Generic)
import           Language.Haskell.TH.Syntax   (ModName (ModName), Name (..),
                                               NameFlavour (NameG),
                                               OccName (OccName),
                                               PkgName (PkgName))
import           GHC.Exts                          (IsString (..))
import Data.Data (Data) 
import           Data.Foldable                   (traverse_)
import Control.Concurrent.STM(swapTVar,TVar,STM) 


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
type Possibly a = forall m. (MonadThrow m) => m a

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

__BUG__ :: SomeException -> a
__BUG__ = error . show

showName :: Name -> String
showName = either show showGUI . fromGlobalName

-- | could have fourth field: @Version@.
data GUI = GUI
 { _guiPackage    :: !Package
 , _guiModule     :: !Module
 , _guiIdentifier :: !Identifier
 } deriving (Show,Read,Eq,Ord,Data,Generic,Hashable)

newtype Package    = Package    String deriving (Show,Read,Eq,Ord,Data,Generic,Hashable)
newtype Module     = Module     String deriving (Show,Read,Eq,Ord,Data,Generic,Hashable)
newtype Identifier = Identifier String deriving (Show,Read,Eq,Ord,Data,Generic,Hashable)

-- | only 'NameG' is global, i.e. is unique modulo package and module.
fromGlobalName :: Name -> Possibly GUI
fromGlobalName (Name (OccName occ) (NameG _ (PkgName pkg) (ModName mod))) = return $ GUI (Package pkg) (Module mod) (Identifier occ)
fromGlobalName (Name (OccName occ) _) = failed occ

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

displayDoc :: Doc -> Text
displayDoc = displayT . renderPretty 1.0 80

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

type Adjacency k n = (n, k, [k])

{- | the maximal cycles of a directed graph (represented as an adjacency list)

wraps 'stronglyConnComp'

>>> :{
let graph = [ ("non recursive",        "N", [])
            , ("self recursive",       "S", ["S"])
            , ("mutually recursive A", "A", ["B"])
            , ("mutually recursive B", "B", ["A","C"])
            , ("mutually recursive C", "C", ["A","S","N"])
            ]
:}

>>> cycles graph
[["self recursive"],["mutually recursive A","mutually recursive B","mutually recursive C"]]

properties:

* the output @[[n]]@ is disjoint i.e. the cycles are maximal
* each output element @n@ comes from the input @Graph n e@ (but not the converse e.g. the output can be empty)
* when input an acyclic graph, the empty list is output (a singleton means the node has an edge to itself i.e. self-recursion) (in particular, the empty graph, lists, trees, DAGs)
* when input a complete graph, the singleton list of (the list of) vertices is output

TODO verify:

@
\as -> Set.fromList (map fst $ as) == Set.fromList (flattenSCCs . stronglyConnComp $ as)
@

i.e. it preserves the exact input nodes. which means that the partial 'find' can be safely assumed total.


-}
cycles :: Ord k => [Adjacency k n] -> [[n]]
cycles = sccs2cycles . stronglyConnComp

sccs2cycles :: [SCC n] -> [[n]]
sccs2cycles = mapMaybe $ \case
 AcyclicSCC _ -> Nothing
 CyclicSCC ns -> Just ns

snoc :: [a] -> a -> [a]
snoc xs x = xs <> [x]

data Address = Address
 { _host :: Host
 , _port :: Port
 } deriving (Show,Read,Eq,Ord,Data,Generic)

newtype Host = Host String deriving (Show,Read,Eq,Ord,Data,Generic)
newtype Port = Port Int    deriving (Show,Read,Eq,Ord,Data,Generic)

localhost :: Host 
localhost = Host "localhost" 

-- | >>> displayAddress$ Address (Host "localhost") (Port 8000)
-- "http://localhost:8000"
displayAddress :: Address -> Text
displayAddress (Address (Host h) (Port p)) = format ("http://"%string%":"%shown) h p

address2baseurl :: Address -> BaseUrl
address2baseurl (Address (Host h) (Port p)) = BaseUrl Http h p 

-- | a natural transformation
type (:~>:) f g = forall x. f x -> g x

-- | @($>) = flip ('<$')@
($>) :: (Functor f) => f a -> b -> f b
($>) = flip (<$)

{- | left-biased maximum.

>>> 'maximumBy' ('comparing' 'snd') [('a',0),('b',0),('c',0)]
('c',0)
>>> maximumByL (comparing snd) [('a',0),('b',0),('c',0)]
('a',0)

-}
maximumByL :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumByL f = foldl1 pick
 where
 pick x y = case f x y of
  GT -> x
  EQ -> x
  LT -> y

{- |

>>> argmax length ["0","abc",""]
"abc"

-}
argmax :: Ord b => (a -> b) -> (NonEmpty a -> a)
argmax f = maximumByL (comparing f)

{- | interleave the effects of @g@ between the effects of @f@, collecting the results of @f@.
-}
interleaving :: (Alternative f) => f a -> f x -> f (NonEmpty a)
interleaving f g = (:|) <$> f <*> many (g *> f)

{- | @(-:) = (,)@

fake dictionary literal syntax:

@
 [ "a"-: 1
 , "b"-: 2
 , "c"-: 1+2
 ]
@

-}
(-:) :: a -> b -> (a,b)
(-:) = (,)
infix 1 -:

{- | for convenience when writing string dicts, let's you make a value equal to its key.

@
 [ "a"-: "b"
 , "b"-: "a"
 , both "c"
 ]
@

evaluates to:

@
 [ "a"-: "b"
 , "b"-: "a"
 , "c"-: "c"
 ]
@

-}
both :: a -> (a, a)
both a = (a, a)

{- | for convenience when writing string dicts, lets you leave keys/values blank

@
filterBlanks
 [ "a"-: "b"
 , "b"-: "a"
 , ""-: "..."
 ]
@

evaluates to:

@
 [ "a"-: "b"
 , "b"-: "a"
 ]
@

-}
filterBlanks :: (IsString k, Eq k) => [(k, v)] -> [(k, v)]
filterBlanks = filter $ \case
 ("",_) -> False
 (_, _) -> True

{- | n-ary (homogeneous) depth-first cross-product. 

>>> let xss = [[1,2,3],[4,5,6]] 
>>> cross xss 
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
>>> length (cross xss)
9
>>> map length (cross xss)
[2,2,2,2,2,2,2,2,2]
>>> length xss
2

@cross = 'sequence'@ 

prop> length (cross xss) == product (map length xss) 

prop> all (\ys -> length xss == length ys) (cross xss)

-}
cross :: [[a]] -> [[a]] 
cross = sequence 

{-| "dirty" the cache.

-}
takeTVar :: TVar (Maybe a) -> STM (Maybe a)
takeTVar var = swapTVar var Nothing 


-- ================================================================ --

makeLenses ''GUI
makePrisms ''Package
makePrisms ''Module
makePrisms ''Identifier

makeLenses ''Address
makePrisms ''Host
makePrisms ''Port


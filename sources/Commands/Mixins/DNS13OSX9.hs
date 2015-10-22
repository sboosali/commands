{-# LANGUAGE RankNTypes, LambdaCase, EmptyCase, TupleSections, TypeFamilies  #-}
{-# LANGUAGE FlexibleContexts, KindSignatures, TypeOperators, ScopedTypeVariables, TemplateHaskell, LiberalTypeSynonyms           #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-| 

-}
module Commands.Mixins.DNS13OSX9
 ( module Commands.Mixins.DNS13OSX9
 , module Commands.Mixins.DNS13OSX9.Types 
 , module Commands.RHS.Types
 , module Commands.Sugar.Keys
 ) where

import Commands.Mixins.DNS13OSX9.Types 
import Commands.RHS.Types
import Commands.Sugar.Keys

import Commands.Frontends.Dragon13
import           Commands.Extra
import           Commands.Munging
import qualified Data.HRefCache.Internal         as HRefCache

import qualified Text.Earley                     as E
import qualified Text.Earley.Grammar             as E
import qualified Text.Earley.Internal            as E
import qualified Data.Text.Lazy as T
import Data.Bifunctor
import Data.Bitraversable
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NonEmpty
import Control.Monad.Catch (MonadThrow (..))
import Control.Comonad.Cofree
import Control.Lens hiding (snoc) 

import Data.Void
import Data.Monoid              ((<>))
import           Data.Char
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.Foldable
import           Data.IORef
import           Data.Proxy
import           Data.Typeable
import           Data.STRef
import           Data.Unique
import           Data.Function                   (on)
import Control.Monad.Trans.State
import qualified Data.List as List
import GHC.Exts (IsString(..))
import           Language.Haskell.TH.Syntax      (Name)
import Control.Exception (SomeException (..))


-- ================================================================ --

renameDNSEarleyFunc
 :: forall z m n1 n2 t f1 f2 a. ((f1 ~ DNSEarleyFunc z n1 t), (f2 ~ DNSEarleyFunc z n2 t))
 => (Applicative m)
 => (forall x. RHS n1 t f1 x -> n1 t f1 x -> m (    n2 t f2 x))
 -> (                       RHS n1 t f1 a -> m (RHS n2 t f2 a))
renameDNSEarleyFunc u = \case
 k@(NonTerminal x r)  ->  NonTerminal <$> u k x <*> go r -- like traverse, except this case
 Terminal i r       ->  pure$ Terminal i r
 Terminals i        ->  pure$ Terminals i 
 Opt  i r           ->  Opt  i <$> go r
 Many i r           ->  Many i <$> go r
 Some i r           ->  Some i <$> go r
 Pure a             ->  pure$ Pure a
 r `Apply` x        ->  Apply <$> go r <*> (case x of
  TreeRHS pRHS rRHS ->  TreeRHS <$> go pRHS <*> go rRHS
  LeafRHS p s       ->  pure$ LeafRHS p s)
 r :<*> r'          ->  (:<*>) <$> go r <*> go r'
 Alter rs           ->  Alter <$> go `traverse` rs
 where
 go :: forall x. RHS n1 t f1 x -> m (RHS n2 t f2 x)
 go = renameDNSEarleyFunc u

renameDNSEarleyRHSST
 :: forall z s n1 n2 t f1 f2 a. ((f1 ~ DNSEarleyFunc z n1 t), (f2 ~ DNSEarleyFunc z n2 t))
 => (forall x. RHS n1 t f1 x -> n1 t f1 x -> ST s (    n2 t f2 x))
 -> ST s                   (RHS n1 t f1 a -> ST s (RHS n2 t f2 a))
renameDNSEarleyRHSST u = unsafeIOToST$ do
 c <- HRefCache.newCache
 -- return$ renameRHS'$ \r1 n r2 -> unsafeIOToST$ do
 return$ renameDNSEarleyFunc$ \r1 n -> unsafeIOToST$ do
  k <- HRefCache.forceStableName r1
  readIORef c >>= (HRefCache.lookupRef k >>> traverse readIORef) >>= \case
   Just y  -> return y          -- cache hit
   Nothing -> do                -- cache miss
    y <- unsafeSTToIO$ u r1 n
    v <- newIORef y
    _ <- atomicModifyIORef' c ((,()) . HRefCache.insertRef k v)
    return y

renameDNSEarleyRHSIO
 :: ((f1 ~ DNSEarleyFunc z n1 t), (f2 ~ DNSEarleyFunc z n2 t))
 => (forall x. RHS n1 t f1 x -> n1 t f1 x -> IO (    n2 t f2 x))
 -> IO                   (RHS n1 t f1 a -> IO (RHS n2 t f2 a))
renameDNSEarleyRHSIO u = do
 c <- HRefCache.newCache
 return$ renameDNSEarleyFunc$ \r1 n -> do
  k <- HRefCache.forceStableName r1
  readIORef c >>= (HRefCache.lookupRef k >>> traverse readIORef) >>= \case
   Just y  -> return y          -- cache hit
   Nothing -> do                -- cache miss
    y <- u r1 n
    v <- newIORef y
    _ <- atomicModifyIORef' c ((,()) . HRefCache.insertRef k v)
    return y

renameRHSST
 :: (forall x. RHS n t f x -> n t f x -> ST s (n' t f x))
 -- :: (forall x. RHS n t f x -> n t f x -> RHS n t f x -> ST s (n' t f x))
 -> ST s (RHS n t f a -> ST s (RHS n' t f a))
-- renameRHSST = undefined
-- renameRHSST u = do
--  c <- HRefCache.newCache
--  return$ renameRHS$ \t x -> do
--   k <- HRefCache.forceStableName t
--   readSTRef c >>= (HRefCache.lookupRef k >>> traverse readSTRef) >>= \case
--    Just y  -> return y
--    Nothing -> do
--     y <- u t x
--     v <- newSTRef y
--     _ <- modifySTRef' c ((,()) . HRefCache.insertRef k v)
--     return y
renameRHSST u = unsafeIOToST$ do
 c <- HRefCache.newCache
 -- return$ renameRHS$ \r1 n r2 -> unsafeIOToST$ do
 return$ renameRHS'$ \r1 n -> unsafeIOToST$ do
  k <- HRefCache.forceStableName r1
  readIORef c >>= (HRefCache.lookupRef k >>> traverse readIORef) >>= \case
   Just y  -> return y          -- cache hit
   Nothing -> do                -- cache miss
    y <- unsafeSTToIO$ u r1 n
    v <- newIORef y
    _ <- atomicModifyIORef' c ((,()) . HRefCache.insertRef k v)
    return y
-- renaming recursive RHS doesn't terminate because: in renaming/traversing the non-terminal, the name is decoupled from the body. the natural transformation should return a pair, and the name and the rhs are both cached. makes sense, as we are caching on the non-terminal (isomorphic to a pair) pointer.


-- ================================================================ --

-- renameRHSToEarley
--  :: forall s r n t f a. ()
--  => ST s (        RHS (ConstName                 n) t f a
--          -> ST s (RHS (EarleyName (E.Rule s r) n) t f a)
--          )
-- renameRHSToEarley = renameRHSST $ \_ (ConstName n) _ -> do
-- renameRHSToEarley :: ST s (RHS (ConstName (_, n)) t (DNSEarleyFunc z (ConstName (_, n)) t) a -> ST s (RHS (EarleyName (E.Rule s r) n) t (DNSEarleyFunc z (EarleyName (E.Rule s r) n) t) a))
renameRHSToEarley
 :: ST s (        DNSEarleyRHS (E.Rule s r) a
         -> ST s (RHS (EarleyName (E.Rule s r) String)
                      Text
                      (DNSEarleyFunc (E.Rule s r)
                      (EarleyName (E.Rule s r) String) Text)
                      a)
         )
renameRHSToEarley = renameDNSEarleyRHSST $ \_ (ConstName (_, n)) -> do
 conts <- newSTRef =<< newSTRef []
 null  <- newSTRef Nothing
 return$ EarleyName (\p -> E.NonTerminal (E.Rule p null conts) (E.Pure id) E.<?> n)

induceEarley
 :: forall s r n t a z. ((z ~ E.Rule s r), (n ~ String))  -- type equality only for documentation
 => (Eq t)
 => RHS (EarleyName z n)
        t
        (DNSEarleyFunc z (EarleyName z n) t)
        a
 -> E.Prod z n t a
induceEarley rhs = runRHSWith
 (\n r -> (unEarleyName n) (induceEarley r))  -- accessor (not pattern match) for polymorphic z (Rank2 elsewhere)
 E.symbol
 (\case
  LeafRHS p    _ -> p
  TreeRHS pRHS _ -> induceEarley pRHS)
 (getTerminalsDNSEarley rhs) 
 rhs 

buildEarley
 :: E.Prod (E.Rule s a) n t a
 -> [t]
 -> ST s (E.Result s n [t] a)
buildEarley p ts = do
  s <- E.initialState p
  E.parse [s] (E.emptyParseEnv ts)

runEarley
 :: (forall r. RHS (ConstName (DNSInfo,String)) Text (DNSEarleyFunc (E.Rule s r) (ConstName (DNSInfo,String)) Text) a)
 -> [Text]
 -> ST s (E.Result s String [Text] a)
-- runEarley
--  :: (Eq t)
--  => (forall r. RHS (ConstName (_,String)) t (DNSEarleyFunc (E.Rule s r) (ConstName (_,String)) t) a)
--  -> [t]
--  -> ST s (E.Result s String [t] a)
runEarley r1 ts = do
 r2 <- renameRHSToEarley >>= ($ r1)
 buildEarley (induceEarley r2) ts

parseRaw
 :: (forall r. RHS (ConstName (DNSInfo,String)) Text (DNSEarleyFunc r (ConstName (DNSInfo,String)) Text) a)
 -> [Text]
 -> ([a], E.Report String [Text])
parseRaw r ts = E.fullParses$ runEarley r ts

e'ParseBest :: (forall r. RULED EarleyParser r a) -> [Text] -> EarleyEither String Text a
e'ParseBest p ts = (p&pBest) <$> e'ParseAll (p&pProd) ts

e'ParseAll :: (forall r. RULED EarleyProd r a) -> [Text] -> EarleyEither String Text (NonEmpty a)
e'ParseAll p ts = toEarleyEither (E.fullParses (buildEarley p ts))
 -- where
 -- report = E.fullParses result
 -- result = buildEarley (p&pProd) ts

e'ParseList :: (forall r. RULED EarleyProd r a) -> [Text] -> [a]
e'ParseList p ts = fst (E.fullParses (buildEarley p ts))

-- | may 'throwM' a @('E.Report' String Text)@
parseThrow
 :: (forall z. DNSEarleyRHS z a)
 -> [Text]
 -> Possibly (NonEmpty a)
parseThrow r = parseEither r >>> \case
 Left  e  -> throwM e
 Right xs -> return xs

parseEither
 :: (forall r. RHS (ConstName (DNSInfo,String)) Text (DNSEarleyFunc r (ConstName (DNSInfo,String)) Text) a)
 -> [Text]
 -> EarleyEither String Text (NonEmpty a)
parseEither r
 = toEarleyEither
 . parseRaw r

parseText
 :: (forall r. DNSEarleyRHS r a) -- Couldn't match type ‘z1’ with ‘r’ because type variable ‘r’ would escape its scope
 -> [Text]
 -> EarleyEither String Text (NonEmpty a)
parseText r = parseEither r --  TODO

parseBest
 :: (NonEmpty a -> a)
 -> (forall r. DNSEarleyRHS r a)
 -> [Text]
 -> EarleyEither String Text a --TODO use Possibly?
parseBest best r = second best . parseText r

parseList :: (forall r. DNSEarleyRHS r a) -> [Text] -> [a]
parseList r ts = as
 where (as,_) = parseRaw r ts

parseString :: (forall r. DNSEarleyRHS r a) -> String -> [a]
parseString r = parseList r . (T.words . T.pack)




-- ================================================================ --

renameRHSToDNS :: IO (RHS (DNSEarleyName n) t (DNSEarleyFunc z (DNSEarleyName n) t) a -> IO (RHS (DNSUniqueName n) t (DNSEarleyFunc z (DNSUniqueName n) t) a))
renameRHSToDNS = renameDNSEarleyRHSIO $ \_ (ConstName (i, n)) -> do
 k <- hashUnique <$> newUnique
 return$ DNSUniqueName i n k

induceDNS
 :: (Eq t)
 => RHS (DNSUniqueName String) t (DNSEarleyFunc z (DNSUniqueName String) t) a
 -> Cofree (DNSRHS t) (DNSInfo, String)
induceDNS = induceDNS' >>> \case
 SomeDNSNonTerminal (DNSRule ((i,n) :< r)) -> (i,n)         :< r
 r                                         -> defaultDNSLHS :< r

defaultDNSLHS :: (DNSInfo,String)
defaultDNSLHS = (defaultDNSInfo,"defaultDNSLHS") -- TODO should be unique; quote it?

induceDNS' -- TODO doesn't terminate on cyclic data
 -- :: RHS (DNSUniqueName n) t (DNSEarleyFunc z (DNSUniqueName n) t) a
 :: (Eq t)
 => RHS (DNSUniqueName String) t (DNSEarleyFunc z (DNSUniqueName String) t) a
 -- -> (Cofree (DNSRHS t) (Maybe (i,n)))
 -> DNSRHS t (Cofree (DNSRHS t) (DNSInfo, String))
induceDNS' rhs = foldRHSWith
  (\(DNSUniqueName i n k) r -> SomeDNSNonTerminal$ DNSRule$ (i, n <> "_" <> (show k)) :< r)
  (DNSTerminal . DNSToken)
  (\case
   LeafRHS _ g -> unVoidDNSRHS g
   TreeRHS _ gRHS -> induceDNS' gRHS) -- auxiliary recursion, not a cata
  (UnitDNSRHS)
  (\r1 r2 -> DNSSequence (r1 :| [r2]))
  (maybe ZeroDNSRHS DNSAlternatives . NonEmpty.nonEmpty)
  (DNSOptional)
  (DNSOptional . DNSMultiple)
  (DNSMultiple)
  (getTerminalsDNSEarley rhs)
  rhs 

unVoidDNSRHS :: DNSRHS t Void -> DNSRHS t n
unVoidDNSRHS = second (\case)

-- 1. collect Cofree's by name
-- 2. for each, project Cofree's to name
reifyDNSRHS :: forall i n t. (Eq n) => Cofree (DNSRHS t) (i,n) -> NonEmpty (DNSProduction i t n)
--  Map n (DNSProduction i t n) is more efficient but unordered
reifyDNSRHS = NonEmpty.fromList            --   TODO prove safety
 . fmap (snd >>> toIndirectDNSRHS >>> toDNSProduction)
 . (flip execState) []
 . go -- runMaybeT
 where

 toIndirectDNSRHS :: Cofree (DNSRHS t) (i,n) -> (i, n, DNSRHS t n)
 toIndirectDNSRHS ((i,n) :< r) = (i,n, bimap id (\((_,n) :< _) -> n) r)

 toDNSProduction :: (i, n, DNSRHS t n) -> DNSProduction i t n
 toDNSProduction (i,n,r) = DNSProduction i (DNSRule n) r

 list'insertBy :: (a -> a -> Bool) -> a -> [a] -> [a]
 list'insertBy eq x xs = case List.find (eq x) xs of
  Nothing -> snoc xs x          -- could reverse later; or like fuse it with a fold; or difference list; performance here doesn't matter
  Just{}  ->    xs

 go :: Cofree (DNSRHS t) (i,n) -> (State [(n, Cofree (DNSRHS t) (i,n))]) ()
 go c@((_,n) :< r) = do
  gets (List.lookup n) >>= \case
   Nothing -> do
    _ <- modify$ list'insertBy ((==) `on` fst) (n, c)
    _ <- bitraverse return go r
    return()
   Just {} -> return()

serializeDNSGrammar' :: DNSGrammar DNSInfo Text Text -> Either [SomeException] SerializedGrammar
serializeDNSGrammar' uG = do
 let oG = optimizeDNSInfoGrammar uG                    -- optimizeDNSInfoGrammar
 eG <- escapeDNSGrammar oG
 let sG = serializeGrammar eG
 return$ sG

formatRHS :: RHS (DNSEarleyName String) Text (DNSEarleyFunc z (DNSEarleyName String) Text) a -> IO (Either [SomeException] SerializedGrammar)
formatRHS r = do
 renamer <- renameRHSToDNS
 let serializeRHS = (induceDNS >>> reifyDNSRHS >>> defaultDNSGrammar >>> second T.pack >>> serializeDNSGrammar')
 eG <- renamer r >>= (serializeRHS >>> return)
 return$ eG

showRHS :: RHS (DNSEarleyName String) Text (DNSEarleyFunc z (DNSEarleyName String) Text) a -> IO Text
showRHS r = do
 eG <- formatRHS r
 return$ either (T.pack . show) displaySerializedGrammar eG


-- ================================================================ --
-- RHS helpers

infix 2 <=>

(<=>) :: Name -> DNSEarleyRHS z a -> DNSEarleyRHS z a
-- type signature for type inference, disambiguates:
--  "No instance for (Data.String.IsString _)" and "No instance for (Functor _)"
(<=>) = genericGrammar

nonterminalGrammar :: String -> DNSEarleyRHS z a -> DNSEarleyRHS z a
nonterminalGrammar l r = NonTerminal (ConstName (defaultDNSInfo, l)) r

genericGrammar :: Name -> DNSEarleyRHS z a -> DNSEarleyRHS z a
genericGrammar name r = nonterminalGrammar (gui^.(guiIdentifier._Identifier)) r
 where Just gui = fromGlobalName name  -- TODO GHC 7.10.2 https://downloads.haskell.org/~ghc/7.10.2/docs/html/users_guide/other-type-extensions.html#special-implicit-params

-- | manually construct a special rule, with primitives.
simpleGrammar :: Name -> (E.Prod z String Text a) -> (DNSRHS Text Void) -> DNSEarleyRHS z a
simpleGrammar n p r = genericGrammar n $ liftLeaf p r

-- | manually construct a special rule, with two independent right-hand sides.
complexGrammar :: Name -> DNSEarleyRHS z a -> DNSEarleyRHS z a -> DNSEarleyRHS z a
complexGrammar n p r = genericGrammar n $ liftTree p r

-- | automatically generate a grammar from a type: the left-hand side comes from the type, and the right-hand side comes from the 'Show'n and transformed 'constructors'.
transformedGrammar :: forall z a. (Typeable a, Enum a, Show a) => (String -> String) -> DNSEarleyRHS z a
transformedGrammar f = nonterminalGrammar
 (guiOf(Proxy :: Proxy a) ^. (guiIdentifier._Identifier))  -- TODO Haskell type sections, whenever
 (asum . fmap (transformedCon f) $ constructors)

-- | helper function for conveniently using Dragon NaturallySpeaking built-ins; sets 'dnsInline' to true.
dragonGrammar :: Name -> (E.Prod z String Text a) -> DNSBuiltinRule -> DNSEarleyRHS z a
dragonGrammar name p r = set (_RHSInfo.dnsInline) True $ simpleGrammar name p (SomeDNSNonTerminal (DNSBuiltinRule r))

-- | a default 'Grammar' for 'Enum's.
--
-- with 'Enum's, we can get the "edit only once" property: edit the @data@ definition, then 'terminal' builds the 'Rule', and then the functions on 'Rule's build the 'Parser's and 'DNSGrammar's. with 'Typeable', but without TemplateHaskell.
--
-- the 'LHS' comes from the type, not the term (avoiding TemplateHaskell). other 'Grammar's can always be defined with an LHS that comes from the term, e.g. with '<=>' (as Haskell values' names are disjoint from Haskell types').
--
--
enumGrammar :: (Typeable a, Enum a, Show a) => DNSEarleyRHS z a
enumGrammar = transformedGrammar (overCamelCase id)

-- | a default 'Grammar' for simple ADTs.
--
-- detects the type name in a constructor name (as a
-- prefix/infix/suffix) and elides the affix.
--
-- useful when you want your @grammars@-DSL terminals to be
-- unqualified (for convenience), but you want your Haskell
-- identifiers to be qualified (to avoid conflicts). e.g.:
--
-- e.g. avoids naming conflicts with @Either@:
--
-- >>> :set -XDeriveDataTypeable
-- >>> data Button = LeftButton | ButtonMiddleButton | ButtonRight deriving (Show,Eq,Enum,Typeable)
-- >>> let button = qualifiedGrammar :: Grammar Button
-- >>> getWords . view gramGrammar $ button
-- ["left","middle","right"]
--
-- (the qualification is exaggerated to show the filtering behavior:
-- it's consistent in idiomatic declarations).
--
-- we didn't define @data Button = Left | Middle | Right@ because it
-- conflicts with 'Either', but the derived grammar is identical.
--
--
--
--
qualifiedGrammar :: forall z a. (Typeable a, Enum a, Show a) => DNSEarleyRHS z a
qualifiedGrammar = qualifiedGrammarWith occ
 where
 occ = guiOf(Proxy :: Proxy a) ^. (guiIdentifier._Identifier)
 -- GUI _ _ (Identifier occ) = guiOf (Proxy :: Proxy a)

-- | a default 'Grammar' for simple ADTs.
--
-- elides the given <http://en.wikipedia.org/wiki/Affix affix> from any part of any constructor.
--
-- e.g. avoids naming conflicts with @Either@. without making either the data type name too short, or the data constructor names too long:
--
-- >>> :set -XDeriveDataTypeable
-- >>> data Direction = UpD | DownD | LeftD | RightD  deriving (Show,Eq,Enum,Typeable)
-- >>> qualifiedGrammarWith "D" :: Grammar Direction
-- ["up","down","left","right"]
--
--
qualifiedGrammarWith :: (Typeable a, Enum a, Show a) => String -> DNSEarleyRHS z a
qualifiedGrammarWith affix = transformedGrammar (overCamelCase (filter (/= fmap toLower affix)))

-- | strips out data typename like 'qualifiedGrammar', and @_@'s, and numbers.
-- makes it easy to generate generic terminals (like @"left"@),
-- without conflicting with.common symbols (like 'Left').
tidyGrammar :: forall z a. (Typeable a, Enum a, Show a) => DNSEarleyRHS z a
tidyGrammar = transformedGrammar (overCamelCase (filter (/= fmap toLower occ)) . filter (/= '_'))
 where
 occ = guiOf(Proxy :: Proxy a) ^. (guiIdentifier._Identifier)

-- | a default 'Grammar' for 'String' @newtype@s.
--
-- the user might want to parse/recognize an arbitrary but dynamic/large subset of all possible strings.
-- For example:
--
-- * a mutable grammer whose contents depend on some context,
-- like the current buffer, or the previous recognition.
-- * a huge list of custom words, that sound like more common words,
-- that aren't being recognized, even after using Dragon's Vocabulary Builder.
-- * even a few static words, which don't need to be a sum typo,
-- to not increase boilerplate, while still increasing type safety.
--
-- e.g.
--
-- @
-- newtype Place = Place String deriving (Show,Eq)
-- instance IsString Place where fromString = Place
-- @
--
--
vocabularyGrammar :: [String] -> DNSEarleyRHS z Text
vocabularyGrammar = tokens

-- | the empty grammar. See 'UnitDNSRHS' (always matches, recognizing nothing) and 'unitEarleyParser' (always succeeds, parsing nothing).
epsilon :: DNSEarleyRHS z ()
epsilon = simpleGrammar 'epsilon unitEarleyParser UnitDNSRHS
 where unitEarleyParser = pure ()

--TODO generalize these introducers to any RHS, and use Text

token :: (IsString t, Show t) => String -> RHS n t f t
token = fromString

str :: String -> DNSEarleyRHS z Text
str = token

vocab :: (IsString t, Show t, Functor'RHS n t f) => [(String, a)] -> RHS n t f a
vocab
 = foldMap (\(s,x) -> x <$ token s)
 . filterBlanks

tokens :: (IsString t, Show t, Functor'RHS n t f) => [String] -> RHS n t f t
tokens = foldMap token

chr :: Char -> DNSEarleyRHS z Char
chr c = c <$ token [c]

-- | a specialization, @int = 'con'@, because integer literals are 'Num'-constrained polymorphic types.
-- in the context we will be using it, we need a concrete type for type inference.
int :: Int -> DNSEarleyRHS z Int
int = con

con :: (Show a) => a -> DNSEarleyRHS z a
con = transformedCon (List.intercalate " " . unCamelCase)

-- | make a 'Terminal' from the @transformed@ 'Show'n constructor, returning the constructor.
transformedCon :: (Show a) => (String -> String) -> a -> DNSEarleyRHS z a
transformedCon f x = x <$ (token . f . show $ x)

-- | @= 'optionRHS' 'enumDefault' ...@
optionalEnum :: (Enum a) => DNSEarleyRHS z a -> DNSEarleyRHS z a
optionalEnum = optionRHS enumDefault



-- ================================================================ --

{- | derive a parser from a DNSEarleyRHS, by observing sharing. 

("d" for DNS, "e" for Earley).

TODO safe with 'unsafePerformIO'?
-}
-- de'deriveParserObservedSharing :: RULED DNSEarleyRHS s a -> ST s (E.Prod (E.Rule s a) String Text a)
de'deriveParserObservedSharing :: RULED DNSEarleyRHS s a -> ST s (RULED EarleyProd s a)
de'deriveParserObservedSharing r1 = do
 r2 <- renameRHSToEarley >>= ($ r1)
 return$ induceEarley r2

{- | derive a grammar from a DNSEarleyRHS, by observing sharing. 

("d" for DNS, "e" for Earley).

-}
de'deriveGrammarObservedSharing :: DNSEarleyRHS z a -> IO SerializedGrammar
de'deriveGrammarObservedSharing rhs = do --TODO may throw exception 
 g <- formatRHS rhs >>= \case
  Right g  -> return g
  Left  es -> throwM$ DNSGrammarException es
 return g


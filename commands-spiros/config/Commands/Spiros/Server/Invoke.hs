{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE #-}

{-|

-}
module Commands.Spiros.Server.Invoke where

-- import Commands.Extra (displayAddress)

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy  as T

import qualified "uglymemo" Data.MemoUgly as Ugly
import qualified "MemoTrie" Data.MemoTrie as Trie
import "MemoTrie" Data.MemoTrie (HasTrie(..), (:->:))
-- import qualified "stable-memo" Data.StableMemo as Stable 

import Control.Arrow (first)

import Prelude.Spiros

--------------------------------------------------------------------------------

-- instance HasTrie Text where
--   newtype (Text :->: b) = TextTrie { unTextTrie :: Reg Text :->: b } 
--   trie      = trieGeneric      TextTrie 
--   untrie    = untrieGeneric    unTextTrie
--   enumerate = enumerateGeneric unTextTrie

-- -- | @Text ~~ [Char]@ 
-- instance HasTrie Text where
--   newtype (Text :->: b) = TextTrie { unTextTrie :: String :->: b } 
--   trie      = trieGeneric      TextTrie 
--   untrie    = untrieGeneric    unTextTrie
--   enumerate = enumerateGeneric unTextTrie

-- | @Text ~~ [Char]@ 
instance HasTrie Text where
  newtype (Text :->: b) = TextTrie { unTextTrie :: String :->: b } 
  trie                 f = TextTrie (trie (T.pack >>> f))
  untrie    (TextTrie t) = T.unpack >>> untrie t 
  enumerate (TextTrie t) = enumerate t & fmap (first T.pack)
  -- TODO should be lazier, or have a direct domain representation, or something 
  
--------------------------------------------------------------------------------

{-| the \"difference\" between each field (in order) of an 'Invocation'.

-}
data Invoker m a = Invoker
  { iParse   :: [Text] -> Maybe a
  , iCompile :: a -> m ()
  }

{-| the fields are lazy, see 'invoke'. 

-}
data Invocation m a = Invocation
 { iRecognized :: [Text]
 , iParsed     :: Maybe a
 , iCompiled   :: m ()
-- , executed   :: Bool
 }

--------------------------------------------------------------------------------

{-| the fields in the output are constructed lazily,
with intermediary computations shared.

a "specialized" function application.

-}
-- handleRequest :: CommandsHandlers a b -> CommandsRequest -> CommandsResponse a b
invoke :: (Applicative m) => Invoker m a -> [Text] -> Invocation m a
invoke Invoker{..} ws = Invocation{..}
 where
 iRecognized = ws
 iParsed     = iRecognized & iParse
 iCompiled   = iParsed & maybe (pure()) iCompile

{-|

@
memoizeInvoker = 'memoizeInvokerViaMap' 
@

-}
memoizeInvoker :: (Ord a) => Invoker m a -> Invoker m a
memoizeInvoker = memoizeInvokerViaMap

-- | via the @uglymemo@ package. 
memoizeInvokerViaMap :: (Ord a) => Invoker m a -> Invoker m a
memoizeInvokerViaMap (Invoker p c) = Invoker{..}
 where 
 iParse   = p & Ugly.memo 
 iCompile = c & Ugly.memo 

-- | via the @MemoTrie@ package.
memoizeInvokerViaTrie :: (Trie.HasTrie a) => Invoker m a -> Invoker m a
memoizeInvokerViaTrie (Invoker p c) = Invoker{..}
 where 
 iParse   = p & Trie.memo 
 iCompile = c & Trie.memo 

{-
-- | via the @memo@ package 
memoizeInvoker :: (Applicative m, Hashable a) => Invoker m a -> Invoker m a
memoizeInvoker (Invoker p c) = Invoker{..}
 iParse   = p & 
 iCompile = c & 
-}


{-# LANGUAGE FlexibleInstances, LambdaCase, TupleSections, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-orphans #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
import qualified Commands.Plugins.Example        as Example
import qualified Commands.Plugins.Example.Phrase as Example
import qualified Commands.Servers.Servant        as Server
-- import qualified Commands.Backends.OSX.Example as OSX
import           Commands.Plugins.Example.Emacs
import qualified Data.RefCache                   as RefCache

import           Data.Reify
import           System.Environment              (getArgs)
-- import Data.Unique
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.Functor.Identity
import           Data.IORef
import           Data.STRef


--
-- $ sleep 2; curl -d '["replace this with that"]' 'http://localhost:8666/recognition/'


main = mainWith =<< getArgs

mainWith = \case
 ["serve"] -> do
  print "serveCommands 8666 root"
  Server.serveNatlink 8666 theModel
 _ -> mainEmacs
 -- _ -> sharingMain
 -- _ -> Example.main
 -- OSX.main


theModel :: Server.CmdModel_ [Example.Phrase_]
theModel = (Server.CmdModel phraseCommand "emacs")


atomicModifyIORef_ f ref = atomicModifyIORef ref ((,()) . f) >> return ref

shared = 0 :: Int
{-# NOINLINE shared #-}

data ListRef a u = EmptyRef | ItemRef a | u `ConsRef` (ListRef a u) deriving Show

instance MuRef Int where
 type DeRef Int = ListRef Int
 mapDeRef _ x = pure (ItemRef x)

instance MuRef (Identity Int) where
 type DeRef (Identity Int) = ListRef (Identity Int)
 mapDeRef _ x = pure (ItemRef x)

instance MuRef (Lazy Int) where
 type DeRef (Lazy Int) = ListRef (Lazy Int)
 mapDeRef _ x = pure (ItemRef x)

instance MuRef [Int] where
 type DeRef [Int] = ListRef Int
 mapDeRef f = \case
  [] -> pure EmptyRef
  (x:xs) -> ConsRef <$> f x <*> mapDeRef f xs

instance MuRef [Identity Int] where
 type DeRef [Identity Int] = ListRef (Identity Int)
 mapDeRef f = \case
  [] -> pure EmptyRef
  (x:xs) -> ConsRef <$> f x <*> mapDeRef f xs

instance MuRef [Lazy Int] where
 type DeRef [Lazy Int] = ListRef (Lazy Int)
 mapDeRef f = \case
  [] -> pure EmptyRef
  (x:xs) -> ConsRef <$> f x <*> mapDeRef f xs

data Lazy a = Lazy a deriving Show
runLazy (Lazy a) = a

sharingMain = do
 putStrLn ""

 -- print =<< reifyGraph [0,0,0::Int]
 -- print =<< reifyGraph [shared,shared,shared]

 print =<< (traverse readIORef <=< traverse (atomicModifyIORef_ (+1)) <=< RefCache.traverseSharedIO newIORef) [0,0,0::Integer]  -- unshared
 -- [1,1,1]

 -- print =<< (traverse readIORef <=< traverse (atomicModifyIORef_ (+1)) <=< RefCache.traverseSharedIO newIORef) [0,0,0::Int]  -- TODO shared by which optimization?
 -- -- [3,3,3]

 print =<< (traverse readIORef <=< traverse (atomicModifyIORef_ (+1)) <=< RefCache.traverseSharedIO newIORef) [shared,shared,shared]  --
 -- [3,3,3]

 print $ runST $ do
  rs <- unsafeIOToST $ RefCache.traverseSharedIO (unsafeSTToIO.newSTRef) [0,0,0::Integer]  -- unshared
  _ <- traverse (\r -> modifySTRef r (+1)) rs
  traverse readSTRef rs
 -- [1,1,1]

 -- unsafeIOToST is about as unsafe as unsafePerformIO
 print $ runST $ do
  rs <- unsafeIOToST $ RefCache.traverseSharedIO (unsafeSTToIO.newSTRef) [shared,shared,shared]  -- shared
  _ <- traverse (\r -> modifySTRef r (+1)) rs
  traverse readSTRef rs
 -- [3,3,3]

 -- print =<< (RefCache.traverseSharedIO (\_ -> hashUnique <$> newUnique)) [0,0,0]
 -- print =<< (RefCache.traverseSharedIO (\_ -> hashUnique <$> newUnique)) [shared,shared,shared]

 -- putStrLn ""
 -- print =<< reifyGraph [shared,shared,shared]
 -- print =<< reifyGraph =<< (RefCache.traverseSharedIO (return.Identity)) [shared,shared,shared]
 -- print =<< reifyGraph =<< ((return . fmap runIdentity) <=< RefCache.traverseSharedIO (return.Identity)) [shared,shared,shared]
 -- print =<< reifyGraph =<< ((return . fmap runLazy)     <=< RefCache.traverseSharedIO (return.Lazy))     [shared,shared,shared]
 -- print =<< reifyGraph =<< (RefCache.traverseSharedIO return) [Identity shared, Identity shared, Identity shared]
 -- print =<< reifyGraph =<< (RefCache.traverseSharedIO return) [Lazy     shared, Lazy     shared, Lazy     shared]


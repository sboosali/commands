{-# LANGUAGE RankNTypes, TupleSections, TypeFamilies #-}
module Commands.Servers.Servant.API where
import           Commands.Backends.OSX          hiding (Application, Command)
import           Commands.Core
import           Commands.Mixins.DNS13OSX9      hiding (left, right)
import           Commands.Servers.Servant.Types

import           Control.Lens
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy.Char8     as BSC
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Wai
import           Servant

import           Control.Monad.IO.Class         (liftIO)


serveNatlink :: (Show a) => Wai.Port -> CmdModel_ a -> IO ()
serveNatlink port cm = Wai.run port $ natlinkApplication cm
-- I think is relevant that ($) is specially typed
-- point-free doesn't work:
--
-- serveNatlink :: (Show a) => Port -> CmdModel_ a -> IO ()
-- serveNatlink port = run port . natlinkApplication
    -- Cannot instantiate unification variable ‘a0’
    -- with a type involving foralls:
    --   forall (z :: * -> * -> * -> *). CmdModel z a
    --   Perhaps you want ImpredicativeTypes

-- makeServantInterpreter :: -> Interpreter
-- makeServantInterpreter

natlinkApplication :: (Show a) => CmdModel_ a -> Wai.Application
natlinkApplication cm = serve natlinkAPI $ natlinkHandlers cm

natlinkAPI :: Proxy NatlinkAPI
natlinkAPI = Proxy

natlinkHandlers :: (Show a) => CmdModel_ a -> Server NatlinkAPI
natlinkHandlers = postRecognition

postRecognition :: (Show a) => CmdModel_ a -> DGNRecognition -> Response ()
postRecognition cm (DGNRecognition ws) = handleInterpret cm $ unwords ws

{- | this handler:

* supports timeout (with TODO). the parsers are fast, even when backtracking exponentially, as it's Haskell and the sentences are short. however, the grammars can be recursively defined, which could result in on termination.
* supports short-circuiting (in 'EitherT'), returning an HTTP error status.
* executes the compiled actions (in 'IO').


-}
handleInterpret :: (Show a) => CmdModel_ a -> String -> Response ()
handleInterpret cm s = do
 liftIO $ putStrLn s
 v <- case ((cm&_modCommand)^.comRule) `parses` s of
  Left  e -> left  err400{errBody = BSC.pack (show e)}
  Right x -> right x
 liftIO $ print v
 let a = ((cm&_modCommand) `compiles` v) (cm&_modContext)
 liftIO $ putStrLn $ showActions a
 liftIO $ runActions a
 return ()

-- handleInterpret :: (Show a) => CmdModel_ a -> String -> Response ()
-- handleInterpret (CmdModel command def context) text = do
--
-- can't bind the rank2 field to command:
--
-- Couldn't match type ‘z0’ with ‘z’
--       because type variable ‘z’ would escape its scope
--     This (rigid, skolem) type variable is bound by
--       a type expected by the context: R z a
--       at sources/Commands/Servers/Servant/API.hs:33:41-72
--     Expected type: C z ApplicationDesugarer Actions_ a
--       Actual type: C z0 ApplicationDesugarer Actions_ a

{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds, LambdaCase, RankNTypes, TypeFamilies, RecordWildCards, TypeOperators  #-}
{- 
a separate module for technical reasons: 
.Types uses DeriveAnyClass, while .V needs GeneralizedNewtypeDeriving, which conflict.
-}
module Commands.Servers.Servant.V where -- TODO V.Core 
import Data.HTypes ((:~>)) 
import           Commands.Extra
import Data.TPrelude (Eff) 
import Commands.Servers.Servant.Types 
import qualified Commands.Frontends.Dragon13 as DNS
import           Commands.Parsers.Earley              (EarleyParser)

-- import Control.Lens
import           Data.Text.Lazy                        (Text)

import           Control.Monad.Trans.Either            (EitherT)
import           Control.Monad.Reader 
import           Control.Monad.Except (MonadError) 
import Control.Concurrent.STM


{-| 

types (parameters, for extensibility): 

* @mb@: the @monad@ for @backend@ affects 
* @c@: the @context@ 
* @v@: the @value@ 


-}
newtype V mb c v a = V { runV :: Eff 

 [ ReaderT (VEnvironment mb c v)
 -- , WorkflowT 
 -- , NatlinkT
 -- , VServerT
 -- , StateT VState
 , EitherT VError 
 ] IO a

 } deriving
 ( MonadReader (VEnvironment mb c v)
 -- , MonadWorkflow
 -- , MonadNatlink
 -- , MonadVServer
 -- , MonadState VState
 , MonadError VError 
 , MonadIO

 , Monad
 , Applicative
 , Functor 
 )


{- | read-only.

"static" configuration.

-}
data VSettings m c v = VSettings
 { vSetup                :: VEnvironment m c v -> IO (Either VError ()) -- TODO VConfig only, with shim
 , vConfig               :: VConfig m c 
 , vPluginRef            :: TVar (VPlugin m c v) 

 , vInterpretRecognition :: VHandler m c v RecognitionRequest
 , vInterpretHypotheses  :: VHandler m c v HypothesesRequest 
 , vInterpretCorrection  :: VHandler m c v CorrectionRequest 
 , vInterpretReload      :: VHandler m c v ReloadRequest     
 , vInterpretContext     :: VHandler m c v ContextRequest    
 }


{- |
-}
data VError = VError String
 deriving (Show,Read,Eq,Ord,Data,Generic)


{-| 

-}
type VHandler m c v i = i -> V m c v DNSResponse
-- newtype VHandler m c a i = VHandler { getVHandler :: VSettings m c a -> i -> Response DNSResponse }  -- contravariant 
-- TODO type VHandlers m c a is = Rec (VHandler m c a) is
-- type VHandler m c v i = VConfig m c v -> Kleisli Response i DNSResponse
-- TODO type VHandlers m c v is = Rec (VHandler m c v) is
-- newtype VHandler m c v i = VHandler { getVHandler :: i -> V m c v DNSResponse }  -- contravariant 


{- | 

configuration

-}
data VEnvironment m c v = VEnvironment 
 { eConfig :: VConfig m c 
 , ePlugin :: VPlugin m c v 
 } 


{- | read-only.

"dynamic" configuration

-}
data VPlugin m c v = VPlugin
 { vGrammar :: DNS.SerializedGrammar
 , vParser  :: (forall s r. EarleyParser s r String Text v) 
 , vDesugar :: c -> v -> m ()
 }

-- type VPlugin c p g d = VPlugin
--  { vGrammar :: g c 
--  , vParser  :: p c 
--  , vDesugar :: d c 
--  }


{-| read-only. (but 'VGlobals' holds mutable references)

"static" configuration.

-}
data VConfig m c = VConfig
 { vSettings_ :: VSettings_
 -- , vFrontend  :: VFrontend DNS.SerializedGrammar
 , vBackend   :: VBackend m 

 , vGlobals   :: VGlobals c       -- not read-only 
 } 


{- | read-only.

simple, "static" configuration.

-}
data VSettings_ = VSettings_
 { vPort                 :: Port
 , vUIAddress            :: Address 
 , vNatLinkSettings      :: DNS.NatLinkConfig 
 -- , vMonitoringAddress     :: Address 
 -- , vEmacs Address     :: Address 
 -- , vChromeAddress     :: Address 
 }


{-| 

mutable references. 

-}
data VGlobals c = VGlobals 
 { vResponse :: TVar DNSResponse -- ^ 
 , vMode :: TVar VMode 
 , vContext :: TVar c 
 -- , vVocabularies :: TVar (Map String [String]) -- TODO safer 
 -- , :: TVar 
 } 
 -- TODO deriving (Generic)

-- type MutableVocabularies n t = TVar (Map n [t]) 

-- type Globals c = Rec TVar [DNSResponse, VMode, c] 
-- type Globals fe c = Rec TVar (c ': FrontendGlobals fe)
-- type Globals fe = Rec TVar (FGlobals fe)
-- type Globals fe be = Rec TVar (FGlobals fe '++ BGlobals be)

-- class Frontend (a :: k) where 
--  type FGlobals a :: [*] 
--  type FGrammar a :: * 

-- instance Frontend Dragon13 where 
--  type FGlobals Dragon13 = [DNSResponse, VMode] 
--  type FGrammar Dragon13 = SerializedGrammar 

-- class Backend (a :: k) where 
--  type BGlobals a :: [*] 

-- I don't think we can really abstract over the frontend/backend. at least, the front ends could need totally different communication channels; we would have to bundle all that together (i.e. for Dragon, the setup script and the shim and the context, while Sphinx might be totally different). 


-- {-| 

-- -}
-- data VFrontend g = VFrontend
--  { vSetup :: VSettings_ -> g -> IO (Either VError ())
--  }


{-| 

-}
data VBackend m = VBackend
 { vExecute :: m :~> IO 
 -- , vCurrentApplication :: m String  -- TODO shouldn't privilege the current application over any other user defined context 
 }


getVEnvironment :: VSettings m c v -> IO (VEnvironment m c v) 
getVEnvironment VSettings{..} = atomically$ do 
 VEnvironment vConfig <$> readTVar vPluginRef



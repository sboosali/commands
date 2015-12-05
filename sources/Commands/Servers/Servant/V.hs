{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds, LambdaCase, RankNTypes, TypeFamilies #-}
{- 
a separate module for technical reasons: 
.Types uses DeriveAnyClass, while .V needs GeneralizedNewtypeDeriving, which conflict.
-}
module Commands.Servers.Servant.V where -- TODO V.Core 

import           Commands.Extra
import Data.TPrelude (Eff) 
import Commands.Servers.Servant.Types 
import qualified Commands.Frontends.Dragon13.Serialize as DNS
import           Commands.Parsers.Earley              (EarleyParser)

-- import Control.Lens
import           Data.Text.Lazy                        (Text)

import           Control.Monad.Trans.Either            (EitherT)
import           Control.Monad.Reader 
import           Control.Monad.Except (MonadError) 
import Control.Concurrent.STM


{-| 

types (parameters, for extensibility): 

* @c@ the "context" 
* @v@ the "value" 


-}
newtype V c v a = V { runV :: Eff 

 [ ReaderT (VSettings IO c v)
 -- , WorkflowT 
 -- , NatlinkT
 -- , VServerT
 -- , StateT VState
 , EitherT VError 
 ] IO a

 } deriving
 ( MonadReader (VSettings IO c v)
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
 { vSettings_            :: VSettings_
 , vSetup                :: VSettings_ -> VConfig m c v -> IO (Either VError ())
 , vConfig               :: VConfig m c v 
 , vGlobals              :: VGlobals c       -- not read-only 
 -- , vUpdateConfig   :: VPlugin :~>: VConfig

 , vInterpretRecognition :: VHandler m c v RecognitionRequest
 , vInterpretHypotheses  :: VHandler m c v HypothesesRequest 
 , vInterpretCorrection  :: VHandler m c v CorrectionRequest 
 , vInterpretReload      :: VHandler m c v ReloadRequest     
 , vInterpretContext     :: VHandler m c v ContextRequest    
 }


{- | read-only.

simple, "static" configuration.

-}
data VSettings_ = VSettings_    -- VAddresses 
 { vServerAddress        :: Address 
 , vUIAddress            :: Address 
 -- , vMonitoringAddress     :: Address 
 -- , vEmacs Address     :: Address 
 -- , vChromeAddress     :: Address 
 }


{-| 

-}
type VHandler m c v i = VSettings m c v -> i -> Response DNSResponse
-- type VHandler m c v i = VGlobals c -> VConfig m c v -> i -> Response DNSResponse
-- newtype VHandler m c a i = VHandler { getVHandler :: VSettings m c a -> i -> Response DNSResponse }  -- contravariant 
-- TODO type VHandlers m c a is = Rec (VHandler m c a) is


{- | read-only.

"dynamic" configuration

-}
data VConfig m c a = VConfig
 { vPlugin  :: VPlugin m c a 
 } 


{- | read-only.

"dynamic" configuration

-}
data VPlugin m c a = VPlugin
 { vGrammar :: DNS.SerializedGrammar
 , vParser  :: (forall s r. EarleyParser s r String Text a) 
 , vDesugar :: c -> a -> m ()
 }


{-| 

-}
data VGlobals c = VGlobals 
 { vResponse :: TVar DNSResponse -- ^ 
 , vMode :: TVar VMode 
 , vContext :: TVar c 
 } 
 -- TODO deriving (Generic)


{- |
-}
data VError = VError String
 deriving (Show,Read,Eq,Ord,Data,Generic)



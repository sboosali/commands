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
import qualified Network.Wai.Handler.Warp              as Wai
import           Data.Text.Lazy                        (Text)

import           Control.Monad.Trans.Either            (EitherT)
import           Control.Monad.Reader 
import           Control.Monad.Except (MonadError) 
import Control.Concurrent.STM


{-| 

types: 

* @c@ the "context"
* @v@ the "value", invariant


-}
newtype V c v a = V { runV :: Eff 

 [ ReaderT (VConfig IO c v) 
 -- , WorkflowT 
 -- , NatlinkT
 -- , VServerT
 -- , StateT VState
 , EitherT VError 
 ] IO a

 } deriving
 ( MonadReader (VConfig IO c v)
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
data VSettings m c a = VSettings
 { vPort                 :: Wai.Port
 , vSetup                :: VSettings m c a -> IO (Either VError ())
 , vInterpretRecognition :: VHandler m c a RecognitionRequest
 , vInterpretHypotheses  :: VHandler m c a HypothesesRequest 
 , vInterpretCorrection  :: VHandler m c a CorrectionRequest 
 , vInterpretReload      :: VHandler m c a ReloadRequest     
 , vInterpretContext     :: VHandler m c a ContextRequest    
 , vConfig               :: VConfig m c a
 , vUIAddress            :: Address 
 , vGlobals              :: VGlobals c 
 -- , vUpdateConfig   :: VPlugin :~>: VConfig
 }


{-| 

-}
type VHandler m c a i = VSettings m c a -> i -> Response DNSResponse
-- newtype VHandler m c a i = VHandler { getVHandler :: VSettings m c a -> i -> Response DNSResponse }  -- contravariant 
-- TODO type VHandlers m c a is = Rec (VHandler m c a) is


{- | read-only.
"dynamic" configuration
-}
data VConfig m c a = VConfig
 { vGrammar :: DNS.SerializedGrammar
 , vParser  :: (forall s r. EarleyParser s r String Text a) 
 , vDesugar :: c -> a -> m ()
 }


{- |
-}
data VError = VError String
 deriving (Show,Read,Eq,Ord,Data,Generic)


{-| 

naming: 

* @c@ for "context". a type parameter, for extensibility. 

-}
data VGlobals c = VGlobals 
 { vResponse :: TVar DNSResponse -- ^ 
 , vMode :: TVar VMode 
 , vContext :: TVar c 
 } 
 -- TODO deriving (Generic)


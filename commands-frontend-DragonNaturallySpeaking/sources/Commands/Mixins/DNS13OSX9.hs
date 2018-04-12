{-# LANGUAGE LambdaCase, RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-| (re-exports)

@
:m +Commands.Mixins.DNS13OSX9
@

-}
module Commands.Mixins.DNS13OSX9
 ( module Commands.Mixins.DNS13OSX9
 , module Commands.Mixins.DNS13OSX9.Types
 , module Commands.Mixins.DNS13OSX9.Derived
 , module Commands.Mixins.DNS13OSX9.Frontend
 , module Commands.Mixins.DNS13OSX9.Parser
 , module Commands.RHS
 , module Commands.Command.Types
 ) where

import Commands.Mixins.DNS13OSX9.Types
import Commands.Mixins.DNS13OSX9.Derived
import Commands.Mixins.DNS13OSX9.Frontend
import Commands.Mixins.DNS13OSX9.Parser
import Commands.RHS
import Commands.Command.Types
import Commands.Frontends.Dragon13 (SerializedGrammar,DnsOptimizationSettings,defaultDnsOptimizationSettings,displaySerializedGrammar)
import           Commands.Parsers.Earley (EarleyParser(..),EarleyEither,fromProd_)

import Data.Text.Lazy (Text)

import Data.Function ((&))
import Control.Monad.Trans.Reader (ReaderT(..))

import Prelude

-- | see 'unsafeInterpretCommand'
data Command' m a = Command'
 { cGrammar :: SerializedGrammar
 , cParser  :: (forall s r. EarleyParser s r String Text a)
 , cRun     :: a -> m ()
 }

--
-- {-|
--
-- -}
-- isFiniteDNSEarleyGrammar :: RHS n t (DNSEarleyFunc n t) a -> IsFiniteGrammar t
-- isFiniteDNSEarleyGrammar = isFiniteGrammar isFiniteDNSEarleyFunc
--
-- {-| only uses the dragon grammar (which stores exact tokens),
-- as the earley parser's Terminal is a predicate (not a token).
--
-- -}
-- isFiniteDNSEarleyFunc :: DNSEarleyFunc n t a -> IsFiniteGrammar t
-- isFiniteDNSEarleyFunc = \case
--  LeafRHS _ g -> isFiniteDNSRHS g
--  TreeRHS _ gRHS -> isFiniteDNSEarleyGrammar gRHS

test_observeParserAndGrammar :: DNSEarleyRHS a -> (String, String -> EarleyEither String Text a)
test_observeParserAndGrammar r =
 ( displaySerializedGrammar (unsafeDNSGrammar defaultDnsOptimizationSettings r)
 , fromProd_ (unsafeEarleyProd r)
 )

{-| interprets an RHS into a grammar and an equivalent parser.
-}
unsafeInterpretCommand
 :: DnsOptimizationSettings
 -> DNSEarleyCommand c (m ()) a
 -> Command' (ReaderT c m) a
unsafeInterpretCommand dnsSettings command = Command'
 (unsafeDNSGrammar dnsSettings (command&_cRHS))
 (EarleyParser (unsafeEarleyProd (command&_cRHS)) (command&_cBest))
 (\a -> ReaderT $ \c -> (command&_cDesugar) c a)

{-

{-| see 'interpretCommand'
-}
unsafeInterpretCommand
 :: DnsOptimizationSettings
 -> DNSEarleyCommand c (m ()) a
 -> Command' (ReaderT c m) a
unsafeInterpretCommand x y = unsafePerformIO $ interpretCommand x y

interpretCommand
 :: DnsOptimizationSettings
 -> DNSEarleyCommand c (m ()) a
 -> IO (Command' (ReaderT c m) a)
interpretCommand dnsSettings command = do
 p <- unsafeSTToIO $ de'deriveParserObservedSharing (command&_cRHS)
 let cParser = EarleyParser p (command&_cBest)
 let cDesugar a = ReaderT $ \c -> (command&_cDesugar) c a
 cGrammar <- de'deriveGrammarObservedSharing dnsSettings (command&_cRHS)
 return $ Command'{..}
-}

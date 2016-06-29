{-# LANGUAGE LambdaCase #-}

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
import Commands.Frontends.Dragon13 (defaultDnsOptimizationSettings,displaySerializedGrammar)
import           Commands.Parsers.Earley (EarleyEither,fromProd_)

import Data.Text.Lazy (Text)


{-| 

-}
isFiniteDNSEarleyGrammar :: RHS n t (DNSEarleyFunc n t) a -> IsFiniteGrammar t
isFiniteDNSEarleyGrammar = isFiniteGrammar isFiniteDNSEarleyFunc 

{-| only uses the dragon grammar (which stores exact tokens), 
as the earley parser's Terminal is a predicate (not a token).  

-}
isFiniteDNSEarleyFunc :: DNSEarleyFunc n t a -> IsFiniteGrammar t
isFiniteDNSEarleyFunc = \case
 LeafRHS _ g -> isFiniteDNSRHS g 
 TreeRHS _ gRHS -> isFiniteDNSEarleyGrammar gRHS

test_observeParserAndGrammar :: DNSEarleyRHS a -> (String, String -> EarleyEither String Text a)
test_observeParserAndGrammar r =
 ( displaySerializedGrammar (unsafeDNSGrammar defaultDnsOptimizationSettings r)
 , fromProd_ (unsafeEarleyProd r)
 )


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
 , module Commands.Sugar.Keys
 ) where

import Commands.Mixins.DNS13OSX9.Types 
import Commands.Mixins.DNS13OSX9.Derived 
import Commands.Mixins.DNS13OSX9.Frontend 
import Commands.Mixins.DNS13OSX9.Parser 
import Commands.RHS
import Commands.Sugar.Keys


{-| only uses the dragon grammar, as the earley parser's Terminal is a predicate (not a token).  

-}
isFiniteDNSEarleyFunc :: DNSEarleyFunc z n t a -> IsFiniteGrammar t
isFiniteDNSEarleyFunc = \case
 LeafRHS _ g -> isFiniteDNSRHS g 
 TreeRHS _ gRHS -> isFiniteDNSEarleyGrammar gRHS

{-| 

-}
isFiniteDNSEarleyGrammar :: RHS n t (DNSEarleyFunc z n t) a -> IsFiniteGrammar t
isFiniteDNSEarleyGrammar = isFiniteGrammar isFiniteDNSEarleyFunc 


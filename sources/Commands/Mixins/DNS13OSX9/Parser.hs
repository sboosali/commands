{-# LANGUAGE RankNTypes, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, LiberalTypeSynonyms, TypeFamilies             #-}

{-| 

-}
module Commands.Mixins.DNS13OSX9.Parser where 

import Commands.RHS
import Commands.Mixins.DNS13OSX9.Types 
import Commands.Mixins.DNS13OSX9.Derived 
import Commands.Mixins.DNS13OSX9.ObservedSharing 
import Commands.Parsers.Earley

import qualified Text.Earley                     as E
import qualified Text.Earley.Internal            as E
import Data.Text.Lazy (Text) 

import           Control.Monad.ST

-- unsafe 
import Unsafe.Coerce 
import           Control.Monad.ST.Unsafe
import           System.IO.Unsafe


{-| 

NOTE unsafe: @unsafeCoerceEarleyProd = 'unsafeCoerce'@

both @s1@ and @r1@ are phantom:

* @s1@ is used as a "state thread" in @ST s1@
* @r1@ is used similarly in the types of "Text.Earley.Internal"

being phantom, and not "reflected" into values (by the uses above), an improper coercion shouldn't segfault. 
however, it may violate referential transparency. 

TODO verify safety conditions 

-}
unsafeCoerceEarleyProd :: E.Prod (E.Rule s1 r1) e t a -> E.Prod (E.Rule s2 r2) e t a 
unsafeCoerceEarleyProd = unsafeCoerce


-- ================================================================ --

-- | NOTE unsafe: can violate referential transparency.   
renameRHSToEarley
 :: ST s (        DNSEarleyRHS a 
         -> ST s (RHS (EarleyName s r String) Text (DNSEarleyFunc (EarleyName s r String) Text) a)
         )
renameRHSToEarley = renameDNSEarleyRHSST $
 \_ (ConstName (_, n)) -> pure$ EarleyName (buildEarleyNonTerminal n) -- TODO maybe the delay is bad 

{-| the core glue between an 'RHS' and an Earley 'E.Prod'uction. 

-}
induceEarley
 :: (Eq t)
 => RHS (EarleyName s r String)
        t
        (DNSEarleyFunc (EarleyName s r String) t)
        a
 -> ST s (E.ProdR s r String t a)
induceEarley rhs = runRHSWithM 
 (\n r -> (unEarleyName n) =<< (induceEarley r))
  -- NOTE "state thread" type variables (i.e. 's') are coerced 
  -- use accessor (unEarleyName) (not pattern match) for polymorphic z (Rank2 elsewhere)
 (pure . E.symbol)
 (\case
  LeafRHS (UnsafeEarleyProduction p)    _ -> pure$ unsafeCoerceEarleyProd p
  TreeRHS pRHS _ -> induceEarley pRHS)
 (getTerminalsDNSEarley rhs) 
 rhs 

{- | derive a parser from a DNSEarleyRHS, by observing sharing. 

("d" for DNS, "e" for Earley).

TODO safety conditions 

-}
-- de'deriveParserObservedSharing :: RULED DNSEarleyRHS s a -> ST s (E.Prod (E.Rule s a) String Text a)
de'deriveParserObservedSharing :: DNSEarleyRHS a -> ST s (E.ProdR s r String Text a)
de'deriveParserObservedSharing r1 = do
 r2 <- renameRHSToEarley >>= ($ r1)
 induceEarley r2

{-| 

NOTE unsafe: calls 'unsafeSTToIO'. 

-}
unsafeEarleyProd :: DNSEarleyRHS a -> (E.ProdR s r String Text a)
unsafeEarleyProd r = unsafePerformIO$ unsafeSTToIO$ de'deriveParserObservedSharing r  -- TODO lol 
{-# NOINLINE unsafeEarleyProd #-}


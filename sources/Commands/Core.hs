{- | only re-exports, for convenience. use the whole package with a single import.

it conflicts with the @lens@ and @parsec@ and @base@ I use, but only a bit:

@
import Control.Lens hiding ((#), (&))
import Control.Applicative hiding (many, optional)
@

you can import this module to your "configuration" module, which should just define several 'Command's anyway.



-}
module Commands.Core
 ( module Commands.Etc
 , module Commands.Symbol.Types
 , module Commands.Grammar
 , module Commands.Munging
 , module Commands.LHS
 -- , module Commands.Parsers.Earley
 ) where

import Commands.Etc
import Commands.Grammar
import Commands.Instances      ()
import Commands.LHS
import Commands.Munging
-- import Commands.Parsers.Earley
import Commands.Symbol.Types

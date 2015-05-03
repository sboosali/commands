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

 , module Commands.Command.Combinator
 , module Commands.Command

 , module Commands.Grammar.Types
 , module Commands.Grammar

 , module Commands.Graph

 , module Commands.Munging

 , module Commands.Sugar

 , module Commands.Parse.Types
 , module Commands.Parse
 , module Commands.Parsec

 , module Control.Alternative.Free.Associated
 ) where

import Commands.Command
import Commands.Command.Combinator
import Commands.Etc
import Commands.Grammar
import Commands.Grammar.Types
import Commands.Graph
import Commands.Instances                  ()
import Commands.Munging
import Commands.Parse
import Commands.Parse.Types
import Commands.Parsec                     hiding (Empty, lower, option,
                                            optional, runParser, tab, upper)
import Commands.Sugar
import Control.Alternative.Free.Associated

module Commands.Render where
-- import Commands.Etc ()
-- import Commands.Grammar.Types
-- import Control.Alternative.Free.Johansen
-- -- import Control.Applicative
-- import Control.Monad.State
-- import Data.Traversable


-- -- | renders a grammar to a serializable format.
-- --
-- -- transforms a possibly-infinite structure with direct references (i.e. recursive definitions) into a certainly-finite structure with indirect references (i.e. 'DNSLHS').
-- grender :: Grammar a -> State [LHS] DNS
-- grender (Terminal _) = return
-- grender (NonTerminal l (Alt rs)) = do

--   <- rrender rs
--  return $

-- -- |
-- rrender :: RHS a -> State [LHS] DNS
-- rrender (Pure _) = return
-- rrender (Alt rs `App` g) = do
--   <- grender g
--   <- rrender rs
--  return $

-- -- |
-- render :: Grammar a -> DNSGrammar
-- render g = evalState (grender g) []

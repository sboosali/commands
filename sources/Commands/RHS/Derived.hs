{-# LANGUAGE ScopedTypeVariables, RankNTypes, LambdaCase #-}

module Commands.RHS.Derived where
-- import Commands.Extra 
-- import Commands.RHS.Types

-- import           Control.Monad 
-- import qualified Data.Map as Map
-- import           Data.Map (Map)
-- import Control.Monad.State
-- import Data.Functor.Classes



-- -- {-| splits RHS into DAGs (by NonTerminal). outputs the FiniteGrammar at each root. 

-- -- -}
-- -- hasFiniteGrammar :: RHS n t f a -> [(n, FiniteGrammar t)]
-- -- hasFiniteGrammar r =
-- --  stronglyConnComp 


-- type ReifiedRHS n t f = Map (Exists (n t f)) (RHS0 n t f)

-- {-| transforms a possibly-infinite structure with direct references (i.e. a recursively-defined 'RHS') into a certainly-finite structure (i.e. a 'Map' \'s values) with indirect references (i.e. its keys).

-- naming: the "reify" means "reifying the Haskell 'reference graph' between right-hand-sides into a Haskell Map". 

-- -}
-- reifyRHS
--  :: forall n t f a. (Ord1 (n t f))
--  -> (forall x. f x -> State (ReifiedRHS n t f) ())
--  => RHS n t f a
--  -> ReifiedRHS n t f
-- reifyRHS fromF r = execState (reifyRHS_ fromF r) Map.empty 

-- reifyRHS_ 
--  :: forall n t f a. (Ord1 (n t f))
--  -> (forall x. f x -> State (ReifiedRHS n t f) ())
--  => RHS n t f a
--  -> State (ReifiedRHS n t f) ()
-- reifyRHS_ fromF r = go 
--  where 

--  go :: RHS n t f a -> State (ReifiedRHS n t f) () 
--  go = \case 

--   NonTerminal n r -> do 
--    visited <- gets $ Map.member (Exists n) 
--    when (not visited) $ do
--      modify $ Map.insert (Exists n) (Exists r) -- TODO what about nested non-terminals?  
--      go r

--   Many i r -> go r
--   Some i r -> go r 
--   Opt  i r -> go r 

--   r `Apply` f     -> (go r) >> (fromF f)
--   r1 :<*>   r2    -> (go r1) >> (go r2) 
--   Alter rs        -> (traverse_ go rs)       

--   Pure a          -> return() 
--   Terminals   i   -> return() 


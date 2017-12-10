{-# LANGUAGE ScopedTypeVariables, RankNTypes, LambdaCase #-}

module Commands.RHS.Derived where
-- import Commands.Extra 
import Commands.RHS.Types

import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty  as NonEmpty
import Data.Semigroup ((<>))

import qualified Data.List as List 
import           Control.Applicative
import           Data.Foldable       (asum)
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

terminal :: t -> RHS n t f t
terminal = Terminal id
{-# INLINE terminal #-}

terminals :: RHS n t f t
terminals = Terminals id
{-# INLINE terminals #-}

-- |
liftRHS :: f a -> RHS n t f a
liftRHS f = Pure id `Apply` f
{-# INLINE liftRHS #-}

-- | zero or one. @(-?) = 'optionalRHS'@
(-?), optionalRHS :: RHS n t f a -> RHS n t f (Maybe a)
(-?) = optionalRHS
optionalRHS = Opt id -- NOTE constructors (rather than methods) avoid Functor constraint 
{-# INLINEABLE (-?) #-}
{-# INLINEABLE optionalRHS #-}

-- | zero or one. @(-?-) = 'flip' 'optionRHS'@
(-?-) :: RHS n t f a -> a -> RHS n t f a
(-?-) = flip optionRHS
optionRHS :: a -> RHS n t f a -> RHS n t f a
optionRHS x = Opt (maybe x id) -- NOTE constructors (rather than methods) avoid Functor constraint 
{-# INLINEABLE (-?-) #-}
{-# INLINEABLE optionRHS #-}

-- | zero or more. @(-*) = 'manyRHS'@
(-*), manyRHS :: RHS n t f a -> RHS n t f [a]
(-*) = manyRHS
manyRHS = Many id -- NOTE constructors (rather than methods) avoid Functor constraint 
{-# INLINEABLE (-*) #-}
{-# INLINEABLE manyRHS #-}

-- | one or more. @(-+) = 'someRHS'@
(-+), someRHS :: RHS n t f a -> RHS n t f (NonEmpty a)
(-+) = someRHS
someRHS = Some id -- NOTE constructors (rather than methods) avoid Functor constraint 
{-# INLINEABLE (-+) #-}
{-# INLINEABLE someRHS #-}

-- | one or more. @(-++) = 'many1RHS'@
--
-- like 'someRHS', but "downcasted" to a list.
(-++), many1RHS :: RHS n t f a -> RHS n t f [a]
(-++) = many1RHS
many1RHS = Some NonEmpty.toList -- NOTE constructors (rather than methods) avoid Functor constraint 
{-# INLINEABLE (-++) #-}
{-# INLINEABLE many1RHS #-}

-- | @(-|-) = 'eitherRHS'@
--
-- a heterogeneous binary 'Alter'.
(-|-), eitherRHS :: RHS n t f a -> RHS n t f b -> RHS n t f (Either a b)
(-|-) = eitherRHS
eitherRHS l r = Alter [Pure Left :<*> l, Pure Right :<*> r] -- NOTE constructors (rather than methods) avoid Functor constraint
-- thanks to quasi-lawfulness, about the same as {{eitherRHS l r = (Left <$> l) <|> (Right <$> r)}} 
{-# INLINEABLE (-|-) #-}
{-# INLINEABLE eitherRHS #-}

-- (-#-) :: (Functor f, Functor (n t f)) => Int -> RHS n t f a -> RHS n t f [a]
-- (-#-) k = traverse id . replicate k -- TODO what is this



-- ================================================================ --

-- | ignores f and n (which may be mutually recursive with the whole) 
getTerminals :: (Eq t) => RHS n t f a -> [t]
getTerminals = getTerminals' (const id) (const [])

-- TODO take the traversal from either the non-terminal or the functor into the right-hand side again. finds any hidden children right-hand sides. 
-- TODO always terminates, even on recursive grammars 
getTerminals'
 :: (Eq t)
 => (forall x. n t f x -> [t] -> [t])
 -> (forall x. f x          -> [t])
 -> RHS n t f a
 -> [t]
getTerminals' fromN fromF
 = List.nub
 . foldRHS' fromN (:[]) fromF [] [] (<>) concat id id id

runRHS
 :: forall n t f g a. (Alternative g, Eq t)
 => (forall x. n t f x -> RHS n t f x -> g x)
 -> (          t                      -> g t)
 -> (forall x. f x                    -> g x)
 -- -> (          [t]                    -> g [t])
 -> RHS n t f a
 -> g a
runRHS fromN fromT fromF rhs 
 = runRHSWith fromN fromT fromF (getTerminals rhs) rhs -- TODO loops on recursive grammars 

runRHSWithM 
 :: forall n t f m g a. (Applicative m, Alternative g, Eq t)
 => (forall x. n t f x -> RHS n t f x -> m (g x))
 -> (          t                      -> m (g t))
 -> (forall x. f x                    -> m (g x))
 -> [t]
 -> RHS n t f a
 -> m (g a)
runRHSWithM fromN fromT fromF ts rhs 
 = runRHSM' fromN fromT fromF ((fmap asum . traverse fromT) ts) rhs -- TODO loops on recursive grammars 

runRHSWith
 :: forall n t f g a. (Alternative g, Eq t)
 => (forall x. n t f x -> RHS n t f x -> g x)
 -> (          t                      -> g t)
 -> (forall x. f x                    -> g x)
 -- -> (          [t]                    -> g [t])
 -> [t]
 -> RHS n t f a
 -> g a
runRHSWith fromN fromT fromF ts rhs
 = runRHS' fromN fromT fromF ((asum . fmap fromT) ts) rhs -- TODO loops on recursive grammars 

runRHSM' 
 :: forall n t f m g a. (Applicative m, Alternative g, Eq t)
 => (forall x. n t f x -> RHS n t f x -> m (g x))
 -> (          t                      -> m (g t))
 -> (forall x. f x                    -> m (g x))
 -> m (g t) -- TODO
 -> RHS n t f a
 -> m (g a)
runRHSM' fromN fromT fromF aTerminal = \case
 Terminals i -> (\t_ -> i <$> t_) <$> aTerminal 
 Terminal    i t -> (\t_ -> i <$> t_) <$> fromT t
 NonTerminal n r ->       fromN n r

 Opt  i x  -> (\y -> i  <$> optional y)   <$> (go x)
 Many i x  -> (\y -> i  <$> many y)   <$> (go x)
 Some i x  -> (\y -> i' <$> some y )  <$>  (go x) where i' = i . NonEmpty.fromList

 Pure a      -> pure$ pure a
 f `Apply` x -> (\f_ x_ -> f_ <*> x_) <$> go f <*> fromF x
 f :<*>    g -> (\f_ g_ -> f_ <*> g_) <$> go f <*> go g
 Alter fs  -> asum <$> (go `traverse` fs)

 where
 go :: forall x. RHS n t f x -> m (g x)
 go = runRHSM' fromN fromT fromF aTerminal 

runRHS'
 :: forall n t f g a. (Alternative g, Eq t)
 => (forall x. n t f x -> RHS n t f x -> g x)
 -> (          t                      -> g t)
 -> (forall x. f x                    -> g x)
 -> g t -- TODO
 -> RHS n t f a
 -> g a

runRHS' fromN fromT fromF aTerminal = \case
 Terminals i -> i <$> aTerminal 
 Terminal    i t -> i <$> fromT t
 NonTerminal n r ->       fromN n r

 Opt  i x  -> i  <$> optional (go x)
 Many i x  -> i  <$> many     (go x)
 Some i x  -> i' <$> some     (go x) where i' = i . NonEmpty.fromList

 Pure a      -> pure a
 f `Apply` x -> go f <*> fromF x
 f :<*>    g -> go f <*> go g
 Alter fs  -> asum (go `map` fs)

 where
 go :: forall x. RHS n t f x -> g x
 go = runRHS' fromN fromT fromF aTerminal 

foldRHS
 :: forall n t f a b. (Eq t)
 => (forall x. n t f x -> b -> b)
 -> (          t                      -> b)
 -> (forall x. f x                    -> b)
 -> b
 -> (b -> b -> b)
 -> ([b] -> b)
 -> (b -> b)
 -> (b -> b)
 -> (b -> b)
 -> RHS n t f a
 -> b
foldRHS fromN fromT fromF unit mul add opt_ many_ some_ rhs
 = foldRHSWith fromN fromT fromF unit mul add opt_ many_ some_ (getTerminals rhs) rhs 

foldRHSWith 
 :: forall n t f a b. (Eq t)
 => (forall x. n t f x -> b -> b)
 -> (          t                      -> b)
 -> (forall x. f x                    -> b)
 -> b
 -> (b -> b -> b)
 -> ([b] -> b)
 -> (b -> b)
 -> (b -> b)
 -> (b -> b)
 -> [t] 
 -> RHS n t f a
 -> b
foldRHSWith fromN fromT fromF unit mul add opt_ many_ some_ ts rhs
 = foldRHS' fromN fromT fromF ((add . map fromT) ts) unit mul add opt_ many_ some_ rhs 

-- | An "unlifted" 'runRHS'. ignores value (in Pure) and transformations in (Terminal/Opt/Many/Some).
--
-- The arguments: @foldRHS fromN fromT fromF unit mul add opt_ many_ some_@
foldRHS'
 :: forall n t f a b. 
    (forall x. n t f x -> b -> b)
 -> (          t                      -> b)
 -> (forall x. f x                    -> b)
 -> b  -- TODO
 -> b
 -> (b -> b -> b)
 -> ([b] -> b)
 -> (b -> b)
 -> (b -> b)
 -> (b -> b)
 -> RHS n t f a
 -> b
foldRHS' fromN fromT fromF aTerminal unit mul add opt_ many_ some_ = \case
 Terminals _ -> aTerminal 
 Terminal    _ t -> fromT t
 NonTerminal n r -> fromN n (go r)
 Opt  _ x  -> opt_  (go x)
 Many _ x  -> many_ (go x)
 Some _ x  -> some_ (go x)
 Pure _      -> unit
 f `Apply` x -> go f `mul` fromF x
 f :<*>    g -> go f `mul` go g
 Alter fs  -> add (go `map` fs)
 where
 go :: forall x. RHS n t f x -> b
 go = foldRHS' fromN fromT fromF aTerminal unit mul add opt_ many_ some_

{- | unwraps a 'NonTerminal', otherwise preserves the input.

when NonTerminals are interpreted as tagging a right-hand side with a left-hand side for "sharing", this function should "unshare" its input from other right-hand side expressions with the same "name".

(the double underscores imply magic; while this function is a magical, it's useful with magical interpretations like observed sharing)

-}
__inlineRHS__ :: RHS n t f a -> RHS n t f a
__inlineRHS__ = \case
 NonTerminal _ r -> r
 r -> r

-- a Traversal?
renameRHS'
 :: forall m n1 n2 t f a. (Applicative m)
 => (forall x. RHS n1 t f x ->     n1 t f x -> m (    n2 t f x))
 -- => (forall x. RHS n1 t f x ->     n1 t f x -> RHS n1 t f x -> m (    n2 t f x , RHS n2 t f x))
 -> (                          RHS n1 t f a ->                 m (RHS n2 t f a))
renameRHS' u = \case
 k@(NonTerminal x r)  ->  NonTerminal <$> u k x <*> go r -- like traverse, except this case
 -- k@(NonTerminal x r)  ->  (uncurry NonTerminal) <$> u k x r
 Terminals i        ->  pure$ Terminals i 
 Terminal i r       ->  pure$ Terminal i r
 Opt  i r           ->  Opt  i <$> go r
 Many i r           ->  Many i <$> go r
 Some i r           ->  Some i <$> go r
 Pure a             ->  pure$ Pure a
 r `Apply` x        ->  Apply  <$> go r <*> pure x -- preserved
 r :<*> r'          ->  (:<*>) <$> go r <*> go r'
 Alter rs           ->  Alter <$> go `traverse` rs
 where
 go :: forall x. RHS n1 t f x -> m (RHS n2 t f x)
 go = renameRHS' u

renameRHS
 :: forall m n1 n2 t f a. (Applicative m)
 => (forall x. RHS n1 t f x ->     n1 t f x -> RHS n1 t f x -> m (    n2 t f x , RHS n2 t f x))
 -> (                          RHS n1 t f a ->                 m (RHS n2 t f a))
renameRHS u = \case
 k@(NonTerminal x r)  ->  uncurry(NonTerminal) <$> u k x r
 Terminals i        ->  pure$ Terminals i 
 Terminal i r       ->  pure$ Terminal i r
 Opt  i r           ->  Opt  i <$> go r
 Many i r           ->  Many i <$> go r
 Some i r           ->  Some i <$> go r
 Pure a             ->  pure$ Pure a
 r `Apply` x        ->  Apply  <$> go r <*> pure x -- preserved
 r :<*> r'          ->  (:<*>) <$> go r <*> go r'
 Alter rs           ->  Alter <$> go `traverse` rs
 where
 go :: forall x. RHS n1 t f x -> m (RHS n2 t f x)
 go = renameRHS u


-- -- | the children
-- selfRHS :: Traversal (RHS n t f a) (RHS n' t' f' a')
-- selfRHS = traverse

-- invmapTerminal :: (t1 -> t2) -> (t2 -> t1) -> RHS n t1 f a -> RHS n t2 f a
-- invmapTerminal fromT intoT = \case
--  Terminal i t -> Terminal (i.intoT) (fromT t)
--  r ->

-- instance ((f' ~ f), (n' ~ n), Functor'RHS n t f) => AppRHS n' t f' (Command n t f c b x) where
--  type LeftRHS (Command n t f c b x) a = (x -> a)
--  appRHS f x = f <*> toRHS x

-- instance IsRHS n t f (Command n t f c b a) where
--   type ToRHS (Command n t f c b a) = a
--   toRHS = _cRHS

-- displayRhs :: () -> () -> () -> () -> RHS n t f a -> String 
-- displayRhs = 

{- 
showRhs                         -- TODO IfConstrained https://hackage.haskell.org/package/ifcxt
 :: forall n t f a. (Show t, Show1 (n t f), Show1 f, Show a)
 => RHS n t f a
 -> String 
showRhs = \case
 Terminals _ -> "Terminals _" 
 Terminal _ t -> printf "Terminal _ (%s)" (show t) 
 NonTerminal n r -> printf "NonTerminal (%s) (%s)" (show1 n) (go r) 

 Opt  _ r -> printf "Opt  _ %s" (go r)
 Many _ r -> printf "Many _ %s" (go r)
 Some _ r -> printf "Some _ %s" (go r)

 Pure a      -> printf "Pure %s" (show a) 
 r `Apply` f -> printf "%s `Apply` %s" (go r) (show f) 
 r1 :<*>  r2 -> printf "%s :<*> %s" (go r1) (go r2) 
 Alter fs  -> printf "Alter %s" (go `map` fs) -- TODO 

 where
 go :: forall x. RHS n t f x -> String
 go = runRHS' fromN fromT fromF aTerminal 
-}


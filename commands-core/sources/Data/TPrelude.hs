{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeFamilies #-}
module Data.TPrelude where 

--import Prelude

{-| n-ary type-level composition. 

fake effect type for declaring monad stacks more cleanly. order matters.

>>> import Control.Monad.State (StateT) 
>>> import Control.Monad.Reader (ReaderT) 
>>> import Control.Monad.Writer (WriterT)
>>> :kind! Eff [StateT Integer, ReaderT Bool, WriterT String] IO a
StateT Integer (ReaderT Bool (WriterT String IO)) a

types: 

* @ts@ is a type-level list of monad transformers 
* @m@ is the base monad 

-}
type family Eff
 (ts :: [(* -> *) -> * -> *])
 (m  ::              * -> *) 
     ::              * -> * 
 where
 Eff '[]       m = m 
 Eff (t ': ts) m = t (Eff ts m) 


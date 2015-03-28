{-# LANGUAGE TypeOperators #-}
module Commands.Compiler.Types where


type Compiler a = a -> CompilerContext -> Actions
-- can cache, as both arguments instantiate Eq

type CompilerContext = String


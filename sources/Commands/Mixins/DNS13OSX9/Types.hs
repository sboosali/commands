{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}
module Commands.Mixins.DNS13OSX9.Types where
import Commands.Grammar.Types
import Commands.Parse.Types
import Commands.Mixins.DNS13.Types
import Commands.Backends.OSX.Types


type C = Command Parser DNSReifying ApplicationDesugarer

type G = Grammar Parser DNSReifying

type R = RHS     Parser DNSReifying

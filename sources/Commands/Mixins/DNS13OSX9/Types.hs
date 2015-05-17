{-# LANGUAGE DeriveFunctor, RankNTypes, TemplateHaskell #-}
module Commands.Mixins.DNS13OSX9.Types where
import Commands.Backends.OSX.Types
import Commands.Frontends.Dragon13.Types
import Commands.LHS
import Commands.Parsers.Earley
import Commands.Symbol.Types


type C z d b = Command      (EarleyProduction z LHS) DNSReifying d LHS String b
type R z     = Rule         (EarleyProduction z LHS) DNSReifying   LHS String
type H z     = RHS          (EarleyProduction z LHS) DNSReifying   LHS String
type S z     = Symbol (Rule (EarleyProduction z LHS) DNSReifying   LHS)String
type P z     =               EarleyProduction z LHS                    String

type C_     = forall z. C z ApplicationDesugarer Actions_
type R_     = forall z. R z
type H_     = forall z. H z
type S_     = forall z. S z


module Main where
import Commands.Test.Properties
import Test.DocTest
import Test.Tasty
import Test.Tasty.QuickCheck


tastytest = defaultMain $ testGroup "QuickCheck"
 [ testProperty "serialized DNSGrammar is valid Python" prop_DNSGrammar
 ]

main = do
 doctest
  [ "sources/Commands/Etc"
  , "sources/Commands/Instances"
  , "sources/Commands/Parse/Types"
  , "sources/Commands/Parse"
  , "sources/Commands/Parsec"
  , "sources/Commands/Munging"
  , "sources/Commands/Frontends/Dragon13"
  , "sources/Commands/Frontends/Dragon13/Types"
  , "sources/Commands/Frontends/Dragon13/Text"
  , "sources/Commands/Plugins/Example"
  ]
 tastytest

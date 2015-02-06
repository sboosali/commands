module Main where
import Test.DocTest


main = doctest
 [ "sources/Commands/Etc"
 , "sources/Commands/Instances"
 , "sources/Commands/Parse/Types"
 , "sources/Commands/Parse"
 , "sources/Commands/Parsec"
 , "sources/Commands/Munging"
 ]

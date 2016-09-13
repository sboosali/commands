{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module Types.Example where
import Types()

{-|
@
stack build && stack exec -- example-commands-server-types
@
-}
main :: IO ()
main = do
 putStrLn "(Types.Example...)"


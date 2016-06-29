{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module DragonNaturallySpeaking.Example where
import DragonNaturallySpeaking()

{-|
@
stack build && stack exec -- example-commands-frontend-DragonNaturallySpeaking
@
-}
main :: IO ()
main = do
 putStrLn "(DragonNaturallySpeaking.Example...)"


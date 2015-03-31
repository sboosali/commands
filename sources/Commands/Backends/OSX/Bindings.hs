{-# LANGUAGE ViewPatterns #-}
module Commands.Backends.OSX.Bindings where
import Commands.Backends.OSX.Bindings.Raw
import Commands.Backends.OSX.Marshall
import Commands.Backends.OSX.Types

import Foreign.C.String                   (peekCString, withCString)


currentApplication :: IO Application
currentApplication = do
 path <- currentApplicationPath
 return path
-- TODO munge, default to Global


-- |
-- TODO Applications whose name/paths have Unicode characters may or may not marshall correctly.
currentApplicationPath :: IO String
currentApplicationPath = objc_currentApplicationPath >>= peekCString

-- |
pressKey :: [Modifier] -> Key -> IO ()
pressKey (encodeModifiers -> flags) (encodeKey -> key) =
 objc_pressKey flags key

-- |
clickMouse :: [Modifier] -> Positive -> MouseButton -> IO ()
clickMouse = undefined
-- clickMouse (MouseClick (encodeModifiers -> flags) (encodePositive -> n) (encodeButton -> button)) = objc_clickMouse

-- |
getClipboard :: IO Contents
getClipboard = objc_getClipboard >>= peekCString

-- |
setClipboard :: Contents -> IO ()
setClipboard s = withCString s objc_setClipboard

-- |
openURL :: URL -> IO ()
openURL s = withCString s objc_openURL

-- |
openApplication :: Application -> IO ()
openApplication _ = return ()


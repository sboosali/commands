module Commands.Munging where

import Data.Char
import Data.List.Split


-- |
-- >>> lower "LOWER"
-- "lower"
--
lower :: String -> String
lower = fmap toLower

-- |
-- >>> unCamelCase "unCamelCase"
-- ["un","camel","case"]
--
-- >>> unCamelCase ""
-- []
--
unCamelCase :: String -> [String]
unCamelCase = fmap lower . (split . dropInitBlank . keepDelimsL . whenElt) isUpper

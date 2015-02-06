module Commands.Munging where

import Data.List.Split

import Data.Char


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

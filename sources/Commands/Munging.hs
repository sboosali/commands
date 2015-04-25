module Commands.Munging where
import Data.List.Split

import Control.Arrow
import Data.Char
import Data.List
import Data.Monoid
 -- TODO intersperse not intercalate?


{- $setup

>>> :set -XViewPatterns

-}

-- |
--
-- >>> squeezeCase ["squeeze","case"]
-- "squeezecase"
--
squeezeCase :: [String] -> String
squeezeCase = intercalate ""

-- |
--
-- >>> spaceCase ["space","case"]
-- "space case"
--
spaceCase :: [String] -> String
spaceCase = intercalate " "

-- |
-- >>> snakeCase ["snake","case"]
-- "snake_case"
--
snakeCase :: [String] -> String
snakeCase = intercalate "_"

-- |
-- >>> dashCase ["dash","case"]
-- "dash-case"
--
dashCase :: [String] -> String
dashCase = intercalate "-"

-- |
-- >>> fileCase ["file","case"]
-- "file/case"
--
fileCase :: [String] -> String
fileCase = intercalate "/"

-- |
-- >>> camelCase ["camel","case"]
-- "camelCase"
--
camelCase :: [String] -> String
camelCase [] = ""
camelCase (word:words) = lower word <> classCase words

-- |
-- >>> classCase ["class","case"]
-- "ClassCase"
--
classCase :: [String] -> String
classCase = fmap capitalize >>> squeezeCase



-- |
-- >>> upper "upper"
-- "UPPER"
--
upper :: String -> String
upper = fmap toUpper

-- |
-- >>> lower "LOWER"
-- "lower"
--
lower :: String -> String
lower = fmap toLower

-- |
-- >>> capitalize "CAPITALIZE"
-- "Capitalize"
--
-- ~ @titlecase@
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : lower xs

-- |
-- >>> uncapitalize "UNCAPITALIZE"
-- "uNCAPITALIZE"
--
uncapitalize :: String -> String
uncapitalize [] = []
uncapitalize (x:xs) = toLower x : xs



-- |
-- >>> unCamelCase "unCamelCase"
-- ["un","camel","case"]
--
-- >>> unCamelCase ""
-- []
--
-- prop> \(uncapitalize -> x) -> (camelCase . unCamelCase) x == x
--
unCamelCase :: String -> [String]
unCamelCase = fmap lower . (split . dropInitBlank . keepDelimsL . whenElt) isUpper

underCamelCase :: ([String] -> [String]) -> (String -> String)
underCamelCase f = (intercalate " ") . f . unCamelCase
-- underCamelCase = dimap (intercalate " ") unCamelCase
-- or "overCamelCase"?

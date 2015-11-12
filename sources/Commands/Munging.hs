{- | 

> -- for doctests 
>>> :set -XViewPatterns

well behaved on ASCII, at least. 
 
-}
module Commands.Munging where

import Data.List.Split

import Control.Arrow
import Data.Char
import Data.List
import Data.Monoid
 -- TODO intersperse not intercalate?
-- TODO handle acronyms (i.e. adjacent uppercase ) correctly 



-- ================================================================ --

{-| >>> upper "upper"
"UPPER"

-}
upper :: String -> String
upper = fmap toUpper

{-| >>> lower "LOWER"
"lower"

-}
lower :: String -> String
lower = fmap toLower

{-| 

>>> capitalize "capitalize"
"Capitalize"

>>> capitalize "CAPITALIZE"
"Capitalize"

~ @titlecase@

-}
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : lower xs

{-| >>> uncapitalize "UNCAPITALIZE"
"uNCAPITALIZE"

-}
uncapitalize :: String -> String
uncapitalize [] = []
uncapitalize (x:xs) = toLower x : xs



-- ================================================================ --

{-| >>> squeezeCase ["squeeze","case"]
"squeezecase"

-}
squeezeCase :: [String] -> String
squeezeCase = delimit ""

{-| 

>>> dashCase ["dash","case"]
"dash-case"

-}
dashCase :: [String] -> String
dashCase = delimit "-"

{-| >>> spaceCase ["space","case"]
"space case"

-}
spaceCase :: [String] -> String
spaceCase = delimit " "

{-| >>> snakeCase ["snake","case"]
"snake_case"

-}
snakeCase :: [String] -> String
snakeCase = delimit "_"

{-| >>> fileCase ["file","case"]
"file/case"

-}
fileCase :: [String] -> String
fileCase = delimit "/"

{-| 

-}
delimit :: String -> [String] -> String
delimit = intercalate 

{-| >>> camelCase ["camel","case"]
"camelCase"

-}
camelCase :: [String] -> String
camelCase [] = ""
camelCase (word:words) = lower word <> classCase words

{-| >>> classCase ["class","case"]
"ClassCase"

-}
classCase :: [String] -> String
classCase = fmap capitalize >>> squeezeCase



-- ================================================================ --

{-| 

>>> unCamelCase "unCamelCase"
["un","camel","case"]

>>> unCamelCase ""
[]

>>> (camelCase . unCamelCase) "unCamelCase"
"unCamelCase"

>>> (unCamelCase . camelCase) ["un","camel","case"] 
["un","camel","case"]

>>> unCamelCase "mtaBidOptimizationUI"
["mta","bid","optimization","ui"]

prop> \(uncapitalize -> s) -> (camelCase . unCamelCase) s == s -- TODO 

-}
unCamelCase :: String -> [String]
unCamelCase = keepAcronyms . split (dropInitBlank . keepDelimsL . whenElt $ isUpper)
 where

 keepAcronyms
  = fmap lower
  . fmap concat
  . groupBy isSingleLetter  

 isSingleLetter :: String -> String -> Bool
 isSingleLetter = (\x y -> length x == 1 && length y == 1)

{-| >>> unClassCase "MTABidOptimizationUI"
["mta","bid","optimization","ui"]

prop> \(isClassCase -> s) -> (classCase . unClassCase) s == s -- TODO 

-}
unClassCase :: String -> [String]
unClassCase = unCamelCase


{-| 

>>> unDashCase "un-dash-case"
["un","dash","case"]

>>> unDashCase "notDashCase"
["notDashCase"]

>>> unDashCase ""
[]

prop> \(isDashCase -> s) -> (dashCase . unDashCase) s == s -- TODO 

-}
unDashCase :: String -> [String]
unDashCase = unDelimit "-" 

{-| 

-}
unSpaceCase :: String -> [String]
unSpaceCase = unDelimit " " 

{-| 

-}
unSnakeCase :: String -> [String]
unSnakeCase = unDelimit "_" 

{-| 

-}
unFileCase :: String -> [String]
unFileCase = unDelimit "/" 

{-| >>> unDelimit "::" "::global::System.Console.WriteLine" 
["global","System.Console.WriteLine"]

>>> ["::global::System.Console.WriteLine"] >>= (unClassCase >=> unDelimit "." >=> unDelimit "::")
["global","system","console","write","line"]

(unClassCase <=< (unDelimit ".") <=< (unDelimit "::")) =<< ["::global::System.Console.WriteLine"]
["global","system","console","write","line"]

-}
unDelimit :: String -> String -> [String]
unDelimit d = split (dropBlanks . dropDelims $ onSublist d)



-- ================================================================ --

{-| >>> overCamelCase id "overCamelCase"
"over camel case"

>>> overCamelCase ((:[]) . (!!1)) "overCamelCase"
"camel"

-}
overCamelCase :: ([String] -> [String]) -> (String -> String)
overCamelCase f = (intercalate " ") . f . unCamelCase
-- overCamelCase = dimap (intercalate " ") unCamelCase

{-| >>> toCamelCase "to camel case" 
"toCamelCase"

-}
toCamelCase :: String -> String
toCamelCase = words >>> camelCase


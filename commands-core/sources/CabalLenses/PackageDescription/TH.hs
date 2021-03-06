-- | (separate module for GHC stage restriction)
module CabalLenses.PackageDescription.TH where

import Control.Lens

import Language.Haskell.TH.Syntax


-- | accepts all fields, and suffixes them with the given suffix
suffixedNamer :: String -> Name -> [Name] -> Name -> [DefName]
suffixedNamer suffix _typeName _fieldNames fieldName =
 [TopName$ mkName (nameBase fieldName ++ suffix)]

-- | a field called @"fieldName"@ makes a lens called @"fieldNameL"@
suffixedFields :: LensRules
suffixedFields = defaultFieldRules & lensField .~ suffixedNamer "L"


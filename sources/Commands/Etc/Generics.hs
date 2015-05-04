{-# LANGUAGE DefaultSignatures, DeriveGeneric, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, PolyKinds, ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies, TypeOperators                        #-}
module Commands.Etc.Generics where
import           Commands.Frontends.Dragon13.Text (isDNSName)

import qualified Data.Text.Lazy                   as T

import           GHC.Generics


{- $setup

>>> :set -XDeriveGeneric
>>> data Tagged a = Tagged { getTag :: String, getThing :: a } deriving Generic
>>> data Weird = Weird { ($$$) :: () } deriving Generic
>>> data Fieldless = Fieldless () () deriving Generic

-}


{- |

>>> instance TypeName (Tagged a)
>>> typeName (undefined :: (Tagged a))
"Tagged"


-}
class TypeName a where
 typeName         :: a -> String
 default typeName :: (Generic a, Datatype d, (D1 d f) ~ (Rep a)) => a -> String
 typeName = datatypeName . from

{- | Silently filters away those (valid) Haskell field names that are invalid
Python identifiers. such as symbols, or the empty string from
field-less Haskell constructors.

>>> instance FieldNames Weird
>>> instance FieldNames Fieldless
>>> fieldNamesPythonic (undefined :: Weird)
[]
>>> fieldNamesPythonic (undefined :: Fieldless)
[]

-}
fieldNamesPythonic :: (FieldNames a) => a -> [String]
fieldNamesPythonic = filter (isDNSName . T.pack) . fieldNames

{- | returns the 'Selector's of a product type, in order:

* the lack of an instance for ':+:' means it rejects sum types statically
* the lack of an instance for 'V1' means it rejects zero-types statically
* the lack of an instance for 'U1' means it rejects unit-types statically

>>> instance FieldNames (Tagged a)
>>> fieldNames (undefined :: (Tagged a))
["getTag","getThing"]

alas:

* the field names are values not types (like not @GHC.TypeLit.Symbol@s), so we can't reject field names by the name (e.g. no @varsym@s)
* we can't even reject a constructor with 'NoSelector', as it seems to be the only instance of 'Selector'.

hence, 'fieldNamesPythonic'.

-}
class FieldNames a where
 fieldNames         :: a -> [String]
 default fieldNames :: (Generic a, FieldNames' (Rep a)) => a -> [String]
 fieldNames = fieldNames' . from

class FieldNames' rep where
 fieldNames' :: rep i -> [String]

instance FieldNames' f => FieldNames' (D1 x f) where  fieldNames' (M1 f) = fieldNames' f
instance FieldNames' f => FieldNames' (C1 x f) where  fieldNames' (M1 f) = fieldNames' f

instance (FieldNames' f, FieldNames' g) => FieldNames' (f :*: g) where
  fieldNames' (f :*: g) = fieldNames' f ++ fieldNames' g

instance (Selector s) => FieldNames' (S1 s h) where
  fieldNames' f = [selName f]


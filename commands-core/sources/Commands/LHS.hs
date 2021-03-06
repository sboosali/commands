{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable, DeriveGeneric, RankNTypes, LambdaCase #-}
module Commands.LHS where
import Commands.Extra

-- import Data.Hashable

import Data.List                  (intercalate)
import Language.Haskell.TH.Syntax (Name)
import Data.Typeable (Typeable)
 

-- |
data LHS
 = LHS    !GUI                  -- ^ for tokens guaranteed unique by Haskell's name resolution modulo package
 | LHSInt !Int                  -- ^ for tokens guaranteed unique by safe/monadic generation
 | LHSApp !LHS [LHS]            -- ^ for reifying @app@lication of higher-order 'Rule's
 deriving (Show,Read,Eq,Ord,Data,Generic)
instance Hashable LHS

bimapLHS :: (GUI -> GUI) -> (Int -> Int) -> (LHS -> LHS) 
bimapLHS fGUI fInt = \case 
 LHS    gui      -> LHS (fGUI gui) 
 LHSInt int      -> LHSInt (fInt int) 
 (l `LHSApp` ls) -> bimapLHS fGUI fInt l `LHSApp` fmap (bimapLHS fGUI fInt) ls 

lhsOfType :: (Typeable a) => proxy a -> LHS
lhsOfType = LHS . guiOf

-- |
lhsFromName :: Name -> Possibly LHS
lhsFromName name = LHS <$> fromGlobalName name

{-| safe on obviously global 'Name's, like reifying a top-level binding:

@
dictation = ... ('unsafeLHSFromName' \'dictation) ...
@

warning: partial function.

-}
unsafeLHSFromName :: Name -> LHS
unsafeLHSFromName name = lhs where Just lhs = lhsFromName name

{-| 'Identifier' for readability, 'hash'/'showHex' for uniqueness/compactness.

>>> showLHS (LHS (GUI (Package "package") (Module "Module.SubModule") (Identifier "identifier")))
"identifier__Module.SubModule__package"

-}
showLHS :: LHS -> String
showLHS (LHS (GUI (Package "") (Module "") (Identifier occ))) = occ
showLHS (LHS (GUI (Package pkg) (Module mod) (Identifier occ))) = intercalate "__" [occ, mod, pkg]
showLHS (LHSInt i) = "i_" <> hashAlphanumeric i
showLHS (l `LHSApp` ls) = intercalate "___" (showLHS l : fmap showLHS ls)
-- TODO for correctness, safely associate LHSApp by checking depth
-- TODO for compactness, keep unique fully qualified identifier, but later render as unqualified identifier with possible compact unique suffix


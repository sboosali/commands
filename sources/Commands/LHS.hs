{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, RankNTypes #-}
module Commands.LHS where
import Commands.Etc

import Data.Hashable

import GHC.Generics                        (Generic)
import           Data.Typeable                       (Typeable)
import           Language.Haskell.TH.Syntax          (Name)
import Data.List (intercalate)
import           Data.Monoid                         ((<>))


-- |
data LHS
 = LHS    !GUI                  -- ^ for tokens guaranteed unique by Haskell's name resolution modulo package
 | LHSInt !Int                  -- ^ for tokens guaranteed unique by safe/monadic generation
 | LHSApp !LHS [LHS]            -- ^ for reifying @app@lication of higher-order 'Rule's
 deriving (Show, Eq, Ord, Generic,Typeable)
instance Hashable LHS


lhsOfType :: (Typeable a) => proxy a -> LHS
lhsOfType = LHS . guiOf

-- |
lhsFromName :: Name -> Possibly LHS
lhsFromName name = do
 gui <- fromName name
 return $ LHS gui

-- | safe on obviously global 'Name's, like reifying a top-level binding:
--
-- @
-- dictation = ... ('unsafeLHSFromName' \'dictation) ...
-- @
--
-- warning: partial function.
--
unsafeLHSFromName :: Name -> LHS
unsafeLHSFromName name = lhs where Just lhs = lhsFromName name

-- | 'Identifier' for readability, 'hash'/'showHex' for uniqueness/compactness.
--
-- >>> showLHS (LHS (GUI (Package "package") (Module "Module.SubModule") (Identifier "identifier")))
-- "identifier__Module.SubModule__package"
--
-- TODO for correctness, safely associate LHSApp by checking depth
--
-- TODO for compactness, keep unique fully qualified identifier, but later render as unqualified identifier with possible compact unique suffix
showLHS :: LHS -> String
showLHS (LHS (GUI (Package "") (Module "") (Identifier occ))) = occ
showLHS (LHS (GUI (Package pkg) (Module mod) (Identifier occ))) = intercalate "__" [occ, mod, pkg]
showLHS (LHSInt i) = "i_" <> hashAlphanumeric i
showLHS (l `LHSApp` ls) = intercalate "___" (showLHS l : fmap showLHS ls)

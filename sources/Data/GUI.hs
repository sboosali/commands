{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns, LambdaCase, DeriveAnyClass, RankNTypes #-}
module Data.GUI where 
import Data.Possibly 

import           Control.Lens                 (makeLenses,makePrisms)
import           Data.Hashable

import           GHC.Generics                 (Generic)
import Data.Data (Data) 
import           Language.Haskell.TH.Syntax   (ModName (ModName), Name (..),
                                               NameFlavour (NameG),
                                               OccName (OccName),
                                               PkgName (PkgName))


-- | could have fourth field: @Version@.
data GUI = GUI
 { _guiPackage    :: !Package
 , _guiModule     :: !Module
 , _guiIdentifier :: !Identifier
 } deriving (Show,Read,Eq,Ord,Data,Generic,Hashable)

newtype Package    = Package    String deriving (Show,Read,Eq,Ord,Data,Generic,Hashable)
newtype Module     = Module     String deriving (Show,Read,Eq,Ord,Data,Generic,Hashable)
newtype Identifier = Identifier String deriving (Show,Read,Eq,Ord,Data,Generic,Hashable)

-- ================================================================ --

{-| 

>>> :set -XTemplateHaskell 
>>> fromGlobalName 'fromGlobalName
GUI {_guiPackage = Package "comma_DQcOV5TDnEq96EyrJpikmg", _guiModule = Module "Data.GUI", _guiIdentifier = Identifier "fromGlobalName"}
-- TODO is "comma_DQcOV5TDnEq96EyrJpikmg" deterministic? 
>>> let localName = () 
>>> fromGlobalName 'localName :: Just GUI 
Nothing 

only 'NameG' is global, i.e. is unique modulo package and module.

-}
fromGlobalName :: Name -> Possibly GUI
fromGlobalName = \case
 Name (OccName occ) (NameG _ (PkgName pkg) (ModName mod)) -> return$ GUI (Package pkg) (Module mod) (Identifier occ)
 Name (OccName occ) _                                     -> failed$ occ

showName :: Name -> String
showName = either show showGUI . fromGlobalName

{-| 

>>> showGUI (GUI (Package "package") (Module "Module.SubModule") (Identifier "identifier"))
"package-Module.SubModule.identifier" 

-}
showGUI :: GUI -> String
showGUI (GUI (Package pkg) (Module mod) (Identifier occ)) = pkg ++ "-" ++ mod ++ "." ++ occ

{-| 

easily define smart constructors, whose error message has a
fully-qualified name for debugging. if you rename the module, the
error message changes automatically. and if you rename the
identifier, you will get a compile time error from Template Haskell
if you don't update the error message
(unless another name is captured).

e.g.

@
natural :: Integer -> Possibly Natural
natural i
 | i >= 0    = return $ Natural i
 | otherwise = failure 'natural
@

-}
failure :: Name -> Possibly a
failure = throwM . userError . showName

-- ================================================================ --

makeLenses ''GUI
makePrisms ''Package
makePrisms ''Module
makePrisms ''Identifier

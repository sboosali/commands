{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable, DeriveGeneric, DeriveFunctor, RecordWildCards, TemplateHaskell #-}
module Commands.Frontends.Dragon13.Shim.Types where
import           Commands.Extra
import Commands.Frontends.Natlink.Types

import           Data.Text.Lazy                  (Text)
import qualified Data.Text.Lazy                  as T
import           Language.Python.Version2.Parser (parseModule)
import           Language.Python.Common.ParseError (ParseError)
import Control.Lens

import Control.Exception (Exception)

import Prelude() 
import Prelude.Spiros


-- | "keyword arguments" for 'getShim'.
data ShimR t = ShimR
 { __rules__      :: t  -- ^ a Python Docstring
 , __lists__      :: t  -- ^ a Python Dict
 , __export__     :: t  -- ^ a Python String
 -- TODO  this stuff below should probably be a separate interpolation, like servant-python
 , __serverHost__  :: t  -- ^ a Python String
 , __serverPort__  :: t  -- ^ a Python Int
 -- , __logFile__     :: t  -- ^ a Python String
 -- , __contextFile__ :: t  -- ^ a Python String
 -- , :: t   -- ^ a Python
 , __properties__ :: t
 } deriving (Show,Read , Eq,Ord,Data,Generic)

-- | "keyword arguments" for 'getShim'.
data SelectionShimR t = SelectionShimR
 { __SelectionShimR_name__        :: t  -- ^ a Python String
 , __SelectionShimR_selectWords__   :: t 
 , __SelectionShimR_throughWords__  :: t 
 -- TODO  this stuff below should probably be a separate interpolation, like servant-python
 , __SelectionShimR_serverHost__  :: t  -- ^ a Python String
 , __SelectionShimR_serverPort__  :: t  -- ^ a Python Int
 , __SelectionShimR_logFile__     :: t  -- ^ a Python String
 , __SelectionShimR_contextFile__ :: t  -- ^ a Python String
 , __SelectionShimR_properties__ :: t

 -- , :: t   -- ^ a Python
 } deriving (Show,Read, Eq,Ord,Functor,Data,Generic)

-- | syntactically correct Python files (when constructed with 'newPythonFile').
newtype PythonFile = PythonFile {getPythonFile :: Text}  deriving (Show,Eq,Ord,Data,Generic)

-- | an 'Exception'
data PythonSyntaxError = PythonSyntaxError ParseError Text deriving (Show,Eq,Ord)
instance Exception PythonSyntaxError


data NatLinkConfig = NatLinkConfig
 { nlAddress     :: Address
 -- , nlLogFile     :: FilePath
 -- , nlContextFile :: FilePath
 , nlGrammarProperties :: GrammarProperties
 }  deriving (Show,Read,Eq,Ord,Data,Generic)

defaultNatLinkConfig :: NatLinkConfig
defaultNatLinkConfig = NatLinkConfig defaultNatlinkAddress defaultGrammarProperties

defaultNatlinkAddress :: Address
defaultNatlinkAddress = Address defaultNatlinkHost defaultNatlinkPort -- TODO why?

defaultNatlinkHost :: Host
defaultNatlinkHost = Host "127.0.0.1" -- TODO why?

defaultNatlinkPort :: Port
defaultNatlinkPort = Port 8888 -- TODO why?

-- ================================================================ --

{-| smart constructor for 'PythonFile'.

make sure that the input is a valid (at least, syntactically correct)
Python file (with 'parseModule'), reports the syntax error otherwise.

may fail with the parse error and the invalid file.

-}
newPythonFile :: Text -> Either PythonSyntaxError PythonFile
newPythonFile s = case parseModule (T.unpack s) "" of
 Right {} -> Right $ PythonFile s
 Left  e  -> Left  $ PythonSyntaxError e s

-- ================================================================ --

makePrisms ''PythonFile

{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable, DeriveGeneric, DeriveFunctor, RecordWildCards #-}
module Commands.Frontends.Dragon13.Shim.Types where
import           Commands.Extra

import           Data.Text.Lazy                  (Text)
import qualified Data.Text.Lazy                  as T
import           Language.Python.Version2.Parser (parseModule)
import           Language.Python.Common.ParseError (ParseError) 


-- | "keyword arguments" for 'getShim'.
data ShimR t = ShimR
 { __rules__      :: t  -- ^ a Python Docstring
 , __lists__      :: t  -- ^ a Python Dict
 , __export__     :: t  -- ^ a Python String
 -- TODO  this stuff below should probably be a separate interpolation, like servant-python
 , __serverHost__ :: t  -- ^ a Python String
 , __serverPort__ :: t  -- ^ a Python Int
 } deriving (Show,Eq,Ord,Functor,Data,Generic)

-- | syntactically correct Python files (when constructed with 'newPythonFile').
newtype PythonFile = PythonFile Text deriving (Show,Eq,Ord,Data,Generic)

-- | an 'Exception'
data PythonSyntaxError = PythonSyntaxError ParseError Text deriving (Show,Eq,Ord)

-- | smart constructor for 'PythonFile'.
--
-- make sure that the input is a valid (at least, syntactically correct)
-- Python file (with 'parseModule'), reports the syntax error otherwise.
--
-- may fail with the parse error and the invalid file 
newPythonFile :: Text -> Either PythonSyntaxError PythonFile
newPythonFile s = case parseModule (T.unpack s) "" of
 Right {} -> Right $ PythonFile s
 Left  e  -> Left  $ PythonSyntaxError e s


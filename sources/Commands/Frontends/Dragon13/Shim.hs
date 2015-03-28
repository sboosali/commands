{-# LANGUAGE DeriveFunctor, QuasiQuotes, RankNTypes, RecordWildCards #-}
module Commands.Frontends.Dragon13.Shim where

import           Commands.Etc

import           Control.Monad.Catch             (SomeException (..), throwM)
import           Data.Text.Lazy                  (Text)
import qualified Data.Text.Lazy                  as T
import           Language.Python.Version2.Parser (parseModule)
import           Text.InterpolatedString.Perl6

import           Data.Monoid                     (Monoid)
import           GHC.Exts                        (IsString)


-- | "keyword arguments" for 'getShim'.
data ShimR t = ShimR
 { __rules__  :: t  -- ^ a Python Docstring
 , __lists__  :: t  -- ^ a Python Dict
 , __export__ :: t  -- ^ a Python String
 , __url__    :: t  -- ^ a Python String
 } deriving (Show,Eq,Ord,Functor)

-- | syntactically correct Python files.
newtype PythonFile = PythonFile Text deriving (Show,Eq,Ord)


-- | smart constructor for 'PythonFile'.
--
-- make sure that the input is a valid (at least, syntactically correct)
-- Python file (with 'parseModule'), reports the syntax error otherwise.
--
-- a Kleisli arrow.
--
newPythonFile :: Text -> Possibly PythonFile
newPythonFile s = case parseModule (T.unpack s) "" of
 Right {} -> return $ PythonFile s
 Left  e  -> throwM $ SomeException e

{- |

e.g.

@
getShim (ShimR "'''_'''" "{'_':'_'}" "'_'" "'_'")
@

the '__export__' must be exported by '__rules__'.

given valid input, output will be a syntactically-valid Python (2.6)
Module, that only depends on the standard library and @natlink@.

some specializations:

@
getShim :: ShimR String -> String
getShim :: ShimR Text   -> Text
getShim :: ShimR Doc    -> Doc
@

inside the 'qq', besides escaping backslashes (@r'\\\\'@ renders as r'\\') and interpolating between bracesa(@{...}@), "what you see is what you get".
-}
getShim :: (IsString t, Monoid t) => ShimR t -> t
getShim ShimR{..} = [qq|


from natlinkutils import (GrammarBase)

# standard library
from urllib2 import urlopen
import time


rules = {__rules__}

lists = {__lists__}

class NarcissisticGrammar(GrammarBase):
    ''' 'Narcissistic' because:

    * load(grammar, allResults=1)  means: every recognition triggers gotResultsObject
    * activate(rule, exclusive=1)  means: deactivate every other non-exclusive rule

    '''

    gramSpec = rules

    def initialize(self):
        self.load(self.gramSpec, allResults=1, hypothesis=1)
        set_lists(self, lists)
        self.activate({__export__}, exclusive=1, noError=1)

    def gotHypothesis(self, words):
        print ""
        print "-  -  -  -  gotHypothesis  -  -  -  -"
        pass

    def gotResultsInit(self, words, results):
        print ""
        print "-  -  -  -  gotResultsInit  -  -  -  -"
        print words

    def gotResultsObject(self, recognitionType, resultsObject):
        words = next(get_results(resultsObject), [])
        text  = munge_and_flatten(words)

        url      = '%s/%s' % ({__url__}, text)
        response = urlopen(url)

        # don't print until the request is sent
        print '---------- gotResultsObject ----------'
        print text
        print (response.getcode(), list(response))


def set_lists(self,ls):
    for (lhs, rhs) in ls.items():
        self.setList(lhs, rhs)

def now():
    return int(time.clock() * 1000)

def first_result(resultsObject):
    return next(get_results(resultsObject), None)

def get_results(resultsObject):
    '''iterators are more idiomatic'''
    try:
        for number in xrange(10):
            yield resultsObject.getWords(number)

    except:
        return

def munge_and_flatten(words):
    '''
    >>> munge_and_flatten(['spell', r'a\\\\spelling-letter\\\\A', r',\\\\comma\\\\comma', r'a\\\\determiner', 'letter'])
    'spell A , a letter'
    '''

    return ' '.join(word.split(r'\\\\')[0] for word in words)


narcissistic_grammar = NarcissisticGrammar()
narcissistic_grammar.initialize()

def unload():
    global narcissistic_grammar
    if narcissistic_grammar:
        narcissistic_grammar.unload()
    narcissistic_grammar = None
|]

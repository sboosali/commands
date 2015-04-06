{-# LANGUAGE DeriveFunctor, QuasiQuotes, RankNTypes, RecordWildCards #-}
-- | (you should read the source to this module if you want to learn about it: just think of it as a config file)
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
 { __rules__      :: t  -- ^ a Python Docstring
 , __lists__      :: t  -- ^ a Python Dict
 , __export__     :: t  -- ^ a Python String
 -- TODO  this stuff below should probably be a separate interpolation, like servant-python
 , __serverHost__ :: t  -- ^ a Python String
 , __serverPort__ :: t  -- ^ a Python Int
 , __clientHost__ :: t  -- ^ a Python String
 , __clientPort__ :: t  -- ^ a Python Int
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

TODO http://stackoverflow.com/questions/11177809/how-to-ping-ubuntu-guest-on-virtualbox
 the Haskell server runs at @('__serverHost__', '__serverPort__')@ on the host,
while the Python client runs at @('__clientHost__', '__clientPort__')@ on the guest.

given valid input, output will be a syntactically-valid Python (2.6)
Module, that only depends on the standard library and @natlink@.

some specializations:

@
getShim :: ShimR String -> String
getShim :: ShimR Text   -> Text
getShim :: ShimR Doc    -> Doc
@

= Implementation

inside the 'qq', besides escaping backslashes (@r'\\\\'@ renders as r'\\') and interpolating between bracesa(@{...}@), "what you see is what you get".

the quasiquote must use @dict(a=1)@ rather than @{'a':1}@ to not conflict with the quasiquoter's interpolation syntax.
-}
getShim :: (IsString t, Monoid t) => ShimR t -> t
getShim ShimR{..} = [qq|

# natlink13 library
from natlinkmain import (setCheckForGrammarChanges)
from natlinkutils import (GrammarBase)

# standard library
import time
from urllib2 import urlopen


# interpolated from "H"askell

H_RULES  = {__rules__}
H_LISTS  = {__lists__}
H_EXPORT = {__export__}

H_SERVER_HOST = {__serverHost__}
H_SERVER_PORT = {__serverPort__}

# H_CLIENT_HOST = {__clientHost__}
# H_CLIENT_PORT = {__clientPort__}



# the grammar

class NarcissisticGrammar(GrammarBase):
    ''' 'Narcissistic' because:

    * load(.., allResults=1)     means: every recognition triggers gotResultsObject
    * load(.., hypothesis=1)     means: every hypothesis, before the recognition, triggers gotHypothesis
    * activate(.., exclusive=1)  means: deactivate every other non-exclusive rule

    (when both flags are set, NarcissisticGrammar.gotResultsObject is called on
    every recognition of every exclusive rule, including this class's rules
    of course, though I only expect this class to be active).

    '''

    gramSpec = H_RULES

    def initialize(self):
        self.set_rules(H_RULES, H_EXPORT)
        self.set_lists(H_LISTS)

    def gotHypothesis(self, words):
        print ""
        print "-  -  -  -  gotHypothesis  -  -  -  -"
        print words

    def gotResultsInit(self, words, results):
        print ""
        print "-  -  -  -  gotResultsInit  -  -  -  -"
        print results

    def gotResultsObject(self, recognitionType, resultsObject):
        words = next(get_results(resultsObject), [])
        text  = munge_and_flatten(words)

        url      = "http://%s:%s/%s" % (H_SERVER_HOST, H_SERVER_PORT, text)
        response = urlopen(url)

        # don't print until the request is sent
        print "---------- gotResultsObject ----------"
        print words
        print (response.getcode(), list(response))

    '''
    must it reload the grammar?
    '''
    # non-override
    def set_rules(self, rules, export):
        self.gramSpec = rules
        self.load(rules, allResults=1, hypothesis=1)
        self.activate(export, exclusive=1, noError=1)

    '''
    must it reload the grammar?
    '''
    # non-override
    def set_lists(self, lists):
        for (lhs, rhs) in lists.items():
            self.setList(lhs, rhs)



# helpers

# current time in milliseconds
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



# boilerplate

GRAMMAR = None # mutable global

def load():
    global GRAMMAR
    setCheckForGrammarChanges(1) # automatically reload on file change (not only when microphone toggles on)
    GRAMMAR = NarcissisticGrammar()
    GRAMMAR.initialize()

def unload():
    global GRAMMAR
    if GRAMMAR:
        GRAMMAR.unload()
    GRAMMAR = None

load()
|]

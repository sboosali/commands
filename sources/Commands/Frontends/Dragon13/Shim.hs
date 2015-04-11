{-# LANGUAGE DeriveFunctor, QuasiQuotes, RankNTypes, RecordWildCards #-}
-- | (you should read the source for documentation: just think of this module as a config file)
module Commands.Frontends.Dragon13.Shim where

import           Commands.Etc
-- import           Commands.Etc.Generics

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

inside the 'qc', besides escaping backslashes (@r'\\\\'@ renders as r'\\') and interpolating between bracesa(@{...}@), "what you see is what you get".

the quasiquote must use @dict(a=1)@ rather than @{'a':1}@ to not conflict with the quasiquoter's interpolation syntax.
-}
getShim :: (IsString t, Monoid t) => ShimR t -> t
getShim ShimR{..} = [qc|


# _commands.py

# natlink13 library
from natlinkmain import (setCheckForGrammarChanges)
from natlinkutils import (GrammarBase)

# standard library
import time
import json
from urllib2 import urlopen


# interpolated from "H"askell

H_RULES  = {__rules__}
H_LISTS  = {__lists__}
H_EXPORT = {__export__}

H_SERVER_HOST = {__serverHost__}
H_SERVER_PORT = {__serverPort__}

server_address = "http://%s:%s/" % (H_SERVER_HOST, H_SERVER_PORT)

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
        self.set_rules(H_RULES, [H_EXPORT])
        self.set_lists(H_LISTS)
        self.doOnlyGotResultsObject = True # aborts all processing after calling gotResultsObject

    # called when speech is detected before recognition begins.
    def gotBegin(self, moduleInfo):
        # handleDGNContextResponse(timeit("/context", urlopen, ("%s/context" % server_address), timeout=0.1))
        # TODO parameterize " Context"

        print
        print
        print "-  -  -  -  gotBegin  -  -  -  -"
        # moduleInfo is just the current window in Windows
        # print moduleInfo

    def gotHypothesis(self, words):
        print
        print "-  -  -  -  gotHypothesis  -  -  -  -"
        print words

    def gotResultsObject(self, recognitionType, resultsObject):
        words = next(get_results(resultsObject), [])
        data  = munge_recognition(words)
        url   = "%s/recognition/" % (server_address,)        # TODO parameterize "recognition"

        try:
            response = timeit(url, urlopen, url=url, data=json.dumps(data), timeout=0.1)
            handleDGNUpdate(self, response)
        except Exception as e:
            print
            print "sending the request and/or handling the response threw:"
            print e

        # don't print until the request is sent the response is handled
        try:
            print
            print "---------- gotResultsObject ----------"
            print "words  =", words
            print "status =", response.getcode()
            print "body   =", response
        except NameError:
            print
        except Exception as e:
            print
            print e

    # for debugging only, shows whether specific rules (rather than the generic dgndictation) are matching the recognition
    # not called when (self.doOnlyGotResultsObject=True)
    def gotResults(self, words, fullResults):
        print
        print "---------- gotResultsObject ----------"
        print "fullResults =", fullResults

    # TODO    must it reload the grammar?
    # TODO    should include export for safety?
    def set_rules(self, rules, exports):
        self.gramSpec = rules
        self.load(rules, allResults=1, hypothesis=1)
        self.set_exports(exports)

    # TODO must it reload the grammar?
    def set_lists(self, lists):
        for (lhs, rhs) in lists.items():
            self.setList(lhs, rhs)

    # activateSet is idempotent, unlike activate
    def set_exports(self, exports):
        self.activateSet(exports, exclusive=1)



# API

def handleDGNUpdate(self, response):
    if not response: # had timed out
        return
    if response.getcode() != 200:
        return

    j = response.readline()
    o = DGNUpdate.fromJSON(j)

    # TODO does order matter? Before/after loading?
    if o is not None:
        if o.dgnRules is not None and o.dgnExports is not None:
            self.set_rules(o.dgnRules, o.dgnExport)
        if o.dgnLists is not None:
            self.set_lists(o.dgnLists)
        if o.dgnRules is None and o.dgnExports is not None: # just switch context
            self.set_exports(o.dgnExports)

class DGNUpdate(object):

    @classmethod
    def fromJSON(cls, j):
        try:
            d = json.loads(j)
            dgnUpdateRules = d["dgnUpdateRules"]
            dgnUpdateExports = d["dgnUpdateExports"]
            dgnUpdateLists = d["dgnUpdateLists"]
            o = cls(dgnUpdateRules, dgnUpdateExports, dgnUpdateLists)
            return o
        except Exception as e:
            print e

    # TODO something dynamic using __dict__ and *args or something:
    # generated code is less readable/debuggable, but the generating code is simpler
    def __init__(self, dgnUpdateRules, dgnUpdateExports, dgnUpdateLists):
        self.dgnUpdateRules = dgnUpdateRules
        self.dgnUpdateExports = dgnUpdateExports
        self.dgnUpdateLists = dgnUpdateLists

    def __repr__(self):
        return "%s(%r,%r,%r)" % (self.__class__.__name__, self.dgnUpdateRules, self.dgnUpdateExports, self.dgnUpdateLists)

    # def toJSON(self):



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

def munge_recognition(words):
    '''
    >>> munge_recognition(['spell', r'a\\\\spelling-letter\\\\A', r',\\\\comma\\\\comma', r'a\\\\determiner', 'letter'])
    ["spell", "A", ",", "a", "letter"]
    '''
    return [word.split(r'\\\\')[0] for word in words]

# http://stackoverflow.com/questions/1685221/accurately-measure-time-python-function-takes
def timeit(message, callback, *args, **kwargs):
    before = time.clock()
    result = callback(*args,**kwargs)
    after = time.clock()
    print message, ': ', (after - before) * 1000, 'ms'
    return result



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

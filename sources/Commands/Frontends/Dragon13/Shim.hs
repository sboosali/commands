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

import           GHC.Exts                        (IsString)


-- | "keyword arguments" for 'getShim'.
data ShimR t = ShimR
 { __rules__      :: t  -- ^ a Python Docstring
 , __lists__      :: t  -- ^ a Python Dict
 , __export__     :: t  -- ^ a Python String
 -- TODO  this stuff below should probably be a separate interpolation, like servant-python
 , __serverHost__ :: t  -- ^ a Python String
 , __serverPort__ :: t  -- ^ a Python Int
 } deriving (Show,Eq,Ord,Functor)

-- | syntactically correct Python files (when constructed with 'newPythonFile').
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

given valid input, output will be a syntactically-valid Python (2.6)
Module, that only depends on the standard library and @natlink@.

>>> let Right{} = newPythonFile (getShim (ShimR "'''rules'''" "{'list':''}" "export" "localhost" "8666"))

the '__export__' must be exported by '__rules__'.

the Haskell server runs at @('__serverHost__', '__serverPort__')@ on the host.

some specializations:

@
getShim :: ShimR String -> String
getShim :: ShimR Text   -> Text
getShim :: ShimR Doc    -> Doc
@

= Implementation

inside the 'qc', "what you see is what you get", besides:

* escaping backslashes (e.g. @r'\\\\'@ renders as r'\\')
* interpolating between braces (e.g. @{...}@ is not a dict). the quasiquote must use @dict(a=1)@ rather than @{'a':1}@ to not conflict with the quasiquoter's interpolation syntax, or escape the first curly brace (e.g. @\{...}@).
 hello are some words detect Unicode thing speak detect Unicode
-}
getShim :: (IsString t, Monoid t) => ShimR t -> t
getShim ShimR{..} = [qc|
#-*- coding: utf-8 -*-
# _commands.py

# natlink13 library
from natlinkmain import (setCheckForGrammarChanges)
from natlinkutils import (GrammarBase)

# standard library
import time
import json
import urllib2
import traceback

# interpolated from "H"askell

H_RULES  = {__rules__}
H_LISTS  = {__lists__}
H_EXPORT = {__export__}
H_SERVER_HOST = {__serverHost__}
H_SERVER_PORT = {__serverPort__}

# e.g.
# H_RULES  = '''<test> exported = \{test};'''
# H_LISTS  = \{'test', ['upcase region']}
# H_EXPORT = 'test'
# H_SERVER_HOST = "192.168.56.1"
# H_SERVER_PORT = '8666'

server_address = "http://%s:%s/" % (H_SERVER_HOST, H_SERVER_PORT)


# the grammar

class NarcissisticGrammar(GrammarBase):
    ''' 'Narcissistic' because:

    * load(.., allResults=1)     means: every recognition triggers gotResultsObject
    * load(.., hypothesis=1)     means: every hypothesis, before the recognition, triggers gotHypothesis
    * activate(.., exclusive=1)  means: deactivate every other non-exclusive rule

    (when both flags are set on load, NarcissisticGrammar.gotResultsObject is called on
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
        # TODO parameterize "context" API

        print
        print
        print "-  -  -  -  gotBegin  -  -  -  -"
        # moduleInfo is just the current window in Windows

    def gotHypothesis(self, words):
        print
        print "---------- gotHypothesis -------------"
        print words

    def gotResultsObject(self, recognitionType, resultsObject):
        print "---------- gotResultsObject ----------"
        words = next(get_results(resultsObject), [])
        data  = munge_recognition(words)
        url   = "%s/recognition/" % (server_address,)        # TODO parameterize "recognition" API

        # print 'resultsObject =',resultsObject
        print 'words =', words
        print 'url =', url
        print 'data =', json.dumps(data)

        try:
            request  = urllib2.Request(url, json.dumps(data), \{"Content-Type": "application/json"})
            response = urllib2.urlopen(request)
            pass
        except Exception as e:
            print
            print "---------- error ------------------"
            print "sending the request and/or handling the response threw:"
            print e
            print traceback.format_exc()

        # don't print until the request is sent the response is handled
        try:
            print
            print "status =", response.getcode()
            print "body   =", response
        except NameError:
            print
        except Exception as e:
            print
            print "---------- error ------------------"
            print e
            print traceback.format_exc()

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
'
    # activateSet is idempotent, unlike activate
    def set_exports(self, exports):
        self.activateSet(exports, exclusive=1)


# API

# TODO             handleDGNUpdate(self, response)
def handleDGNUpdate(self, response):
    pass


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

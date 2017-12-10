{-# LANGUAGE QuasiQuotes, RecordWildCards #-}
-- | (you should read the source for documentation: just think of this module as a config file)
module Commands.Frontends.Dragon13.Shim.Selection where
import Commands.Frontends.Dragon13.Shim.Types

import           Text.InterpolatedString.Perl6

import           GHC.Exts                        (IsString)

{-|

@
A select XYZ grammar is a special grammar which
recognizes an utterance of the form "<select> <text> [ <through> <text> ]"
where <select> and <through> can both be lists of keywords
and <text> is an arbitrary sequence of words in a specified text buffer.
@

-}
getSelectionShim :: (IsString t, Monoid t) => SelectionShimR t -> t
getSelectionShim SelectionShimR{..} = [qc|
#-*- coding: utf-8 -*-
# _selection.py

DEBUG = True  

# natlink13 library
from natlinkmain import (setCheckForGrammarChanges)
from natlinkutils import (SelectGramBase)
import natlink  # a DLL

# python standard library
import time
import json
import urllib2
import traceback
from collections import (namedtuple)

################################################################################
# TYPES

Properties = namedtuple('Properties', ['status', 'exclusivity', 'shouldEavesdrop', 'shouldHypothesize'])

################################################################################
# UTILITIES

# current time in milliseconds
def now():
    return int(time.clock() * 1000)

def toDragonText(s): 
    return s.decode('utf8').encode('cp1252')

################################################################################
# INTERPOLATIONS from "H"askell

''' 
H_NAME          = {__SelectionShimR_name__}
H_SELECT_WORDS  = {__SelectionShimR_selectWords__} 
H_THROUGH_WORDS = {__SelectionShimR_throughWords__}
H_SERVER_HOST   = {__SelectionShimR_serverHost__} 
H_SERVER_PORT   = {__SelectionShimR_serverPort__} 
H_PROPERTIES    = {__SelectionShimR_properties__}
'''

# example for debugging, overrides previous assignments  
if DEBUG:
    print "DEBUG is True" 
    H_PROPERTIES = Properties(status=True, exclusivity=1, shouldEavesdrop=0, shouldHypothesize=0)
    H_SELECT_WORDS = ["select"]
    H_THROUGH_WORDS = ["until"] 

################################################################################
# THE GRAMMAR

'''

def load( self, selectWords=None, throughWord='through',throughWords=None,allResults=0, hypothesis=0 ):

''' 
class MySelectionGrammar(SelectGramBase):

    def __init__(self):
        SelectGramBase.__init__(self)
        self.load (selectWords=H_SELECT_WORDS , throughWords=H_THROUGH_WORDS , allResults=H_PROPERTIES.shouldEavesdrop, hypothesis=H_PROPERTIES.shouldHypothesize)
        self.activate (exclusive=H_PROPERTIES.exclusivity)

    def update(self,selectWords,throughWords):
        print "MySelectionGrammar.update" 

    def gotBegin(self,moduleInfo):
        print "MySelectionGrammar.gotBegin" 

    def gotResultsObject(self,recogType,resObj):
        print "MySelectionGrammar.gotResultsObject" 

    def gotResults(self,words,start,end):
        print "" 
        print '[MySelectionGrammar] recognition' 
        print (start, end) 
        print words 

################################################################################
# TESTING 

'''
e.g. "select Omnibian until symbol" should work 
''' 
def testMySelectionGrammar(grammar): 
    print "testMySelectionGrammar..." 

    text = ''' here is an example buffer with a weird word like Omnibian

    and with punctuation, like this symbol ":" and the above double newline.  
    ''' 
    
    text = toDragonText ( u'the peculiar Omnibian')

    grammar.setSelectText(text)

    print 
    print '[text]' 
    print grammar.getSelectText() 

################################################################################
# BOILERPLATE

# mutable global
GRAMMAR = None

def load():
    global GRAMMAR
    # automatically reload on file change (not only when microphone toggles on)
    setCheckForGrammarChanges(1)
    GRAMMAR = MySelectionGrammar() 
    if DEBUG: 
        testMySelectionGrammar(GRAMMAR) 
    # GRAMMAR.initialize()

def unload():
    global GRAMMAR
    if GRAMMAR:
        GRAMMAR.unload()
    GRAMMAR = None

load()
################################################################################

''' TODO 

    def gotResultsObject(self,recogType,resObj):
        debug.trace('SelectWinGramNL.gotResultsObject', '** invoked, resObj=%s' % repr(resObj))
        if recogType == 'self':
            utterance = sr_interface.SpokenUtteranceNL(resObj)
            self.results_callback(utterance)
            debug.trace('SelectWinGramNL.gotResultsObject', '** recogType = self')        
            # If there are multiple matches in the text we need to scan through
            # the list of choices to find every entry which has the highest.
            
            ranges = []        
            try:
                bestScore = resObj.getWordInfo(0)[0][2]
                verb = resObj.getWordInfo(0)[0][0]
                #
                # Collect selection ranges with highest score
                #
                for i in range(100):
                    #
                    # The candidate regions are sorted from best to worst scores.
                    # Loop through candidate regions until we reach one whose
                    # score is not the same as the first score (or until a
                    # natlink.outOfRange exception is raised to signal the end
                    # of the list of candidate regions).
                    #
                    wordInfo = resObj.getWordInfo(i)
#                    debug.trace('SelectWinGramNL.gotResultsObject', '** i=%s, len(wordInfo)=%s' % (i, len(wordInfo)))
#                    debug.trace('SelectWinGramNL.gotResultsObject', '** i=%s, len(wordInfo[0])=%s' % (i, len(wordInfo[0])))                    
                    if wordInfo[0][2] != bestScore:
                        #
                        # All remaining regions don't have as good a score as the
                        # first ones.
                        break
                    else:
                        #
                        # This region has the same score as the first ones. Add it
                        # to the candidate selection ranges.
                        #
                        #QH: if a results object is not of this Select Grammar
                        #also break
                        try:
                            region = resObj.getSelectInfo(self.gramObj, i)
                        except natlink.WrongType:
                            continue
                        if debug.tracing('SelectWinGramNL.gotResultsObject'):
                            debug.trace('SelectWinGramNL.gotResultsObject', 'adding region=%s' % repr(region))
                        true_region = (region[0] + self.vis_start,
                          region[1] + self.vis_start)
                        #
                        # For some reason, NatSpeak may return duplicate ranges
                        #
                        if not true_region in ranges:
                           ranges.append(true_region)

            except natlink.OutOfRange, exceptions.IndexError:
                pass

            spoken = self.selection_spoken_form(resObj)
            debug.trace('SelectWinGramNL.gotResultsObject', 'verb=%s, spoken=%s, ranges=%s' % (verb, spoken, repr(ranges)))
            self.find_closest(verb, spoken, ranges)

'''
|]

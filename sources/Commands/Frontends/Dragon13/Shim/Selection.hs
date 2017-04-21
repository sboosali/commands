{-# LANGUAGE QuasiQuotes, RecordWildCards #-}
-- | (you should read the source for documentation: just think of this module as a config file)
module Commands.Frontends.Dragon13.Shim.Selection where
import Commands.Frontends.Dragon13.Shim.Types

import           Text.InterpolatedString.Perl6

import           GHC.Exts                        (IsString)

{-|

-}
getSelectionShim :: (IsString t, Monoid t) => SelectionShimR t -> t
getSelectionShim SelectionShimR{..} = [qc|

#
class SelectionGrammar(SelectGramBase):

    def __init__(self):
        SelectGramBase.__init__(self)

    def update(self,selectWords,throughWords):
        self

    def gotBegin(self,moduleInfo):
        pass

    def gotResultsObject(self,recogType,resObj):
        pass

    def gotResults(self,words,start,end):
        pass

|]

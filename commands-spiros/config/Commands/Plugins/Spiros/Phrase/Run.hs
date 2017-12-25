{-# LANGUAGE LambdaCase, FlexibleInstances, FlexibleContexts, NoMonomorphismRestriction  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults -fno-warn-orphans #-} -- TODO orphans? 
module Commands.Plugins.Spiros.Phrase.Run where
import Commands.Plugins.Spiros.Phrase.Types
import Commands.Plugins.Spiros.Phrase.Munging
import           Commands.Plugins.Spiros.Extra

import           Commands.Plugins.Spiros.Phrase.Spacing
import Commands.Backends.Workflow 

import           Data.List.NonEmpty               (NonEmpty (..))

import qualified Data.List as List 
import           Control.Monad ((>=>)) 


runPhrase_ :: Spacing -> ClipboardText -> Phrase -> String
runPhrase_ spacing clipboard
 = flip(mungePhrase) spacing
 . flip(splatPasted) clipboard
 . pPhrase
 . unPhrase

-- | 

-- runDictation :: (MonadWorkflow m) => Dictation -> m () 
runDictation = \case
 Dictation ws -> insert (List.intercalate " " ws)

-- | 

-- runLetters :: (MonadWorkflow m) => Letters -> m () 
runLetters = insertL

-- runLetters
-- insertL :: (MonadWorkflow m) => Letters -> m () 
insertL (Letters cs) = insert cs 

-- slotD :: (MonadWorkflow m) => Dictation -> m ()
slotD p = do
 delay 10
 insertD p
 press "<ret>"

-- insertD :: (MonadWorkflow m) => Dictation -> m () 
insertD = mungeDictation >>> insert

-- slotP :: (MonadWorkflow m) => Phrase -> m ()
slotP p = do
 delay 10
 insertP p
 press "<ret>"

-- insertP :: (MonadWorkflow m) => Phrase -> m () 
insertP = munge >=> insert

-- munge :: (MonadWorkflow m) => Phrase -> m String
munge (Phrase p1) = do
 p2 <- splatPasted (pPhrase p1) <$> getClipboard
 return$ mungePhrase p2 defSpacing


-- ================================================================ --

bestPhrase :: NonEmpty Phrase -> Phrase
bestPhrase = argmax rankPhrase

-- no list-generic instance, provides flexibility without OverlappingInstances  
instance Rankable Phrase where rank = rankPhrase
-- instance Rankable (Maybe Phrase) where rank = maybe defaultRank rankPhrase

instance Rankable Phrase_ where rank = rankPhrase_ 
instance Rankable Dictation where rank = rankDictation 

rankPhrase :: Phrase -> Int
rankPhrase = sum . fmap rankPhrase_ . unPhrase

-- the specificity ("probability") of the phrase parts. bigger is better.
rankPhrase_ :: Phrase_ -> Int
rankPhrase_ = \case
 Escaped_ _    -> 2 * highRank
 Quoted_ _     -> 2 * highRank
 Pasted_       -> defaultRank
 Clipboard_    -> defaultRank
 Blank_        -> defaultRank
 Capped_ cs    -> highRank + 3 * rankChars cs -- NOTE to recognize "caps mta" 
 Spelled_ cs   -> highRank + 2 * rankChars cs
 Symbol_ cs    -> highRank + 2 * rankChars cs
 Separated_ _  -> defaultRank
 Bonked_       -> defaultRank
 Cased_ _      -> defaultRank
 Joined_ _     -> defaultRank
 Surrounded_ _ -> defaultRank
 Splitted_ _   -> defaultRank 
 Dictated_ d   -> rankDictation d

rankChars cs = length cs - 1

rankDictation (Dictation ws) = length ws - 1
-- [Dictated_ ["some","words"]] =1 is better than [Dictated_ ["some"], Dictated_ ["words"]] =0

-- -- | convenience function for testing how phrase_ parses
-- parsePhrase_ :: [Text] -> String
-- parsePhrase_
--  = runPhrase_ defSpacing "clipboard contents"
--  . argmax rankPhrase
--  . NonEmpty.fromList --  TODO
--  . parseList phrase_


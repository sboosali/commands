{-# LANGUAGE LambdaCase #-}

{-| 

> -- for doctests 
>>> :set -XOverloadedStrings 

-}
module Commands.Plugins.Spiros.Phrase.Munging where
import           Commands.Plugins.Spiros.Phrase.Types
import           Commands.Plugins.Spiros.Phrase.Spacing

import qualified Commands.Backends.Workflow as W
import           Commands.Munging
import           Data.Sexp

import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NonEmpty
import Data.List.Split hiding (Splitter) 

import qualified Data.List               as List
import Data.Monoid                           ((<>))
import           Data.Foldable                    (Foldable (..))
import Data.Char


-- | splats the Pasted into PAtom's, after splitting the clipboard into words
splatPasted :: UPhrase -> W.ClipboardText -> MPhrase
splatPasted p clipboard = either (substPasted clipboard) (:[]) <$> p

substPasted :: String -> Pasted -> [PAtom]
substPasted pasted = \case
 Pasted True  -> PWord <$> words pasted
 Pasted False -> [PText pasted]

mungePhrase :: MPhrase -> Spaced String
mungePhrase p = concatPAtoms =<< evalSplatSexp applyPFunc p

mungeDictation :: Dictation -> String
mungeDictation (Dictation ws) = unwords ws 

{-| >>> mungeDictation (spacedDictation "hello world") 
"hello world "

-}
spacedDictation :: Dictation -> Dictation 
spacedDictation = (<> Dictation [""])

{- |


>>> concatPAtoms ["",""]
PWord ""
>>> concatPAtoms ["",""]
PWord ""she 

-}
concatPAtoms :: [PAtom] -> Spaced String
concatPAtoms xs e
 = (flip spaceOut) e
 . fmap mungePAtom
 $ xs

mungePAtom :: PAtom -> String
mungePAtom = \case
 PWord          x  -> x
 PText          x  -> x
 PAcronym False cs -> lower cs
 PAcronym True  cs -> upper cs

applyPFunc :: [PAtom] -> PFunc -> Spaced [PAtom]
applyPFunc as = \case
  Cased      g -> traverse (return . caseWith g) as
  Joined     g -> return [joinWith g as]
  Surrounded g -> return (surroundWith g as)
  Splitted   g -> return (splitWith g as) 

caseWith :: Casing -> (PAtom -> PAtom)
caseWith c = mapPAtom (fromCasing c)

fromCasing :: Casing -> (String -> String)
fromCasing = \case
 UpperCase  -> upper
 LowerCase  -> lower
 CapCase    -> capitalize

mapPAtom :: (String -> String) -> (PAtom -> PAtom)
mapPAtom f = \case
 PWord    x    -> PWord    (f x)  
 PText    x    -> PText    x          -- ignore transformation 
 PAcronym b cs -> PAcronym b (f cs) -- TODO "copper mike tango alpha" doesn't work; it's "mta", should be "MTA".   
 -- PWord . f . mungePAtom

joinWith :: Joiner -> ([PAtom] -> PAtom)
joinWith = \case
 -- Joiner s    -> List.interleave (PWord s)
 Joiner s     -> PWord . List.intercalate s . fmap mungePAtom
 CamelJoiner  -> PWord . camelAtoms
 ClassJoiner  -> PWord . classAtoms
 ShrinkJoiner -> PWord . shrinkAtoms

camelAtoms :: [PAtom] -> String
camelAtoms []     = ""
camelAtoms (x:xs) = lower (mungePAtom x) <> (classAtoms xs)

classAtoms :: [PAtom] -> String
classAtoms = squeezeCase . fmap go
 where
 go = \case
  PWord w       -> capitalize w
  PText    x    -> x          -- ignore transformation 
  PAcronym _ cs -> upper cs -- TODO distinguish Capped from Acronym to preserve capitalization?

shrinkAtoms :: [PAtom] -> String
shrinkAtoms = filter isAlphaNum . concatMap mungePAtom

surroundWith :: Brackets -> ([PAtom] -> [PAtom])
surroundWith (Brackets l r) as = [PWord l] <> as <> [PWord r]
 -- xs <- traverse mungePAtom as
-- TODO generalize by renaming surround to transform: it shares the type with Interleave
-- e.g. "par thread comma 123" -> (1,2,3)

splitWith :: Splitter -> ([PAtom] -> [PAtom])
splitWith Splitter = concatMap go 
 where
 go = \case 
  PWord w -> PWord <$> splitToken w
  a -> [a] 
  -- PAcronym _ cs -> PWord [cs] 

splitToken :: String -> [String]
splitToken
 = fmap lower
 . filter anyBlack
 . concatMap unAnyCase
 . filter anyBlack
 . concatMap (splitWhen isPunctuation) -- undo joining 
 . wordsBy isSpace 
 where 
 anyBlack = not . all isSpace   -- NOTE {{all _ "" == True}}

{-| 

>>> unAnyCase "unCamelCase"
["un","Camel","Case"]

>>> unAnyCase "ClassCase"
["Class","Case"]

-- preserves acronyms 
>>> unAnyCase "MTABidOptimization"
["MTA","Bid","Optimization"]

>>> unAnyCase ""
[]

-}
unAnyCase :: String -> [String]
unAnyCase
 = mergeAdjacentUpper
 . (split . dropInitBlank . keepDelimsL . whenElt) isUpper

{-| 

>>> mergeAdjacentUpper ["M","T","A","Bid","Optimization"]
["MTA","Bid","Optimization"]

-}

mergeAdjacentUpper :: [String] -> [String]
mergeAdjacentUpper
 = fmap concat
 . List.groupBy (\x y -> isSingleUpper x && isSingleUpper y) 
 where 
 isSingleUpper = \case
  [c] -> isUpper c
  _ -> False 

-- | parses "tokens" into an "s-expression". a total function.
pPhrase :: [Phrase_] -> UPhrase
pPhrase
 = fromStack
 . foldl' go ((Nothing, []) :| [])
 . mergeSpelled

 -- e.g. (PSexp (PList [PAtom (PWord "")]))

 where
 go :: PStack -> Phrase_ -> PStack
 go ps = \case

  (Escaped_  (Keyword x))    -> update ps          $ (fromPAtom . PWord) x
  (Quoted_   (Dictation xs)) -> update ps          $ List ((fromPAtom . PWord) <$> xs) 
  (Dictated_ (Dictation xs)) -> update ps          $ List ((fromPAtom . PWord) <$> xs) 
  (Spelled_  cs)             -> update ps          $ (fromPAtom . PAcronym False) cs
  (Capped_   cs)             -> update ps          $ (fromPAtom . PAcronym True)  cs
  (Symbol_   cs)             -> update ps          $ (fromPAtom . PWord)          cs
  Pasted_                    -> update ps          $ fromPasted True                        
  Clipboard_                 -> update ps          $ fromPasted False 
  Blank_                     -> update ps          $ (fromPAtom . PWord) ""             
  Bonked_                    -> update (popall ps) $ (fromPAtom . PWord) " "
  Separated_ (Separator x)   -> update (pop ps)    $ (fromPAtom . PWord) x

  (Cased_      f)            -> push ps $ Cased f
  (Joined_     f)            -> push ps $ Joined f
  (Surrounded_ f)            -> push ps $ Surrounded f
  (Splitted_   f)            -> push ps $ Splitted f

 popall :: PStack -> PStack
 -- breaks out of every func to the left (like having enough Separated_'s) 
 -- i.e. close the S expression with as many right parenthesis as it takes "...)))"
 popall (p:|(q:qs)) = popall (update (q:|qs) (fromItem p))
 -- can't pop any more
 popall stack = stack

 pop :: PStack -> PStack
 -- break from the innermost PFunc, it becomes an argument to the outer PFunc
 -- i.e. close the S expression with a right parenthesis "...)"
 pop (p:|(q:qs)) = update (q:|qs) (fromItem p)
 -- if too many breaks, just ignore
 pop stack = stack

 -- i.e. open a left parenthesis with some function "(f ..."
 push :: PStack -> PFunc -> PStack
 push (p:|ps) f = (Just f, []) :| (p:ps)

 update :: PStack -> UPhrase -> PStack
 update ((f,ps):|qs) p = (f, ps <> [p]) :| qs

 -- right-associate the PFunc's.
 fromStack :: PStack -> UPhrase
 fromStack = fromItem . foldr1 associateItem . NonEmpty.reverse

 associateItem :: PItem -> PItem -> PItem
 associateItem (f,ps) gqs = (f, ps <> [fromItem gqs])

 fromItem :: PItem -> UPhrase
 fromItem (Nothing, ps) = List   ps
 fromItem (Just f,  ps) = Sexp f ps

 fromPasted :: Bool -> UPhrase 
 fromPasted = Atom . Left . Pasted

 fromPAtom :: PAtom -> UPhrase
 fromPAtom = Atom . Right

-- | merge adjacent 'Spelled_' 
mergeSpelled :: [Phrase_] -> [Phrase_]
mergeSpelled = foldr' go []
 where
 go :: Phrase_ -> [Phrase_] -> [Phrase_]
 go (Spelled_ xs) (Spelled_ ys : ps) = (Spelled_ $ xs <> ys) : ps
 go p ps = p:ps


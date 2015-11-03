{-# LANGUAGE ScopedTypeVariables, RankNTypes, LambdaCase, BangPatterns, TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving #-}

module Commands.RHS.Finite where
import Commands.Extra 
import Commands.RHS.Types
-- import Commands.RHS.Derived 

-- import Derive.List 

import Data.Semigroup

import qualified Data.List as List
import           Control.Applicative
import           Control.Monad 
import Data.Maybe 
-- import GHC.Exts (IsList, IsString(..) )


{-| an 'RHS' subset that is finite and homogeneous. 

to be finite: 

* it lacks grammatical repetition. i.e. 'Some'/'Many'.  
* its fields are all (TODO) strict. you must not define FiniteGrammar values recursively. 


-}
data FiniteGrammar t
 = FiniteTerminal !t
 | FiniteOptional !(FiniteGrammar t)
 | FiniteSequence !(FiniteGrammar t) !(FiniteGrammar t)
 | FiniteAlternatives ![FiniteGrammar t] -- TODO strict list import Data.Sequence (Seq) 
 deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable,Data,Generic)

-- deriveSemigroup ''FiniteGrammar 'FiniteAlternatives 

{-| the outer Maybe provides failability (with its Applicative instance). the inner Maybe provides an identity (with its Monoid instance).

(see <https://byorgey.wordpress.com/2011/04/18/monoids-for-maybe/ monoids for maybe>)

-}
type IsFiniteGrammar t = Maybe (Maybe (FiniteGrammar t))

{-| a grammatical sentence. 

-}
newtype Sentence t = Sentence [t]
 deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable,Semigroup,Monoid,Data,Generic) -- TODO custom IsList and IsString

unSentence :: Sentence t -> [t]
unSentence (Sentence ts) = ts 

toSentence :: String -> Sentence String 
-- toSentence :: (IsString t) => t -> Sentence t
toSentence = Sentence . words

{-| >>> displaySentence ["this", "is", "a", "sentence"] 
"this is a sentence" 

-}
displaySentence :: Sentence String -> String
displaySentence = List.intercalate " " . unSentence

printSentence :: Sentence String -> IO ()
printSentence sentence = do 
 putStrLn $ displaySentence sentence 

{- | outputs a grammar that is finite and homogeneous. 

-}
isFiniteGrammar
 :: forall t n f a. ()
 => (forall x.         f x -> IsFiniteGrammar t)
 -> (          RHS n t f a -> IsFiniteGrammar t) 

isFiniteGrammar fromF = \case

 Many{} -> abortIsFiniteGrammar
 Some{} -> abortIsFiniteGrammar

 Terminals{} -> ignoreIsFiniteGrammar         -- NOTE being the set of terminals, it should add no new terminals 
 Pure{}      -> ignoreIsFiniteGrammar

 Terminal    _ t -> (keepIsFiniteGrammar FiniteTerminal) t 
 NonTerminal _ r -> go r
 r `Apply` f     -> (when2_ FiniteSequence)     <$> (go r) <*> (fromF f)
 r1 :<*>   r2    -> (when2_ FiniteSequence)     <$> (go r1) <*> (go r2)
 Alter rs        -> (whenN_ FiniteAlternatives) <$> (traverse go rs)
 Opt  _ r        -> (when1_ FiniteOptional)     <$> (go r)

 where

 go :: forall x. RHS n t f x -> IsFiniteGrammar t
 go = isFiniteGrammar fromF

 -- keep (as the given constructor), only when the grammar may be finite. 
 when1_ f = fmap f 
 when2_ f = liftA2 f 
 whenN_ f = Just . f . catMaybes

{-| abort (the grammar is infinite) -}
abortIsFiniteGrammar :: IsFiniteGrammar t
abortIsFiniteGrammar = Nothing

{-| ignore (the grammar has no subgrammars, but is finite) -}
ignoreIsFiniteGrammar :: IsFiniteGrammar t
ignoreIsFiniteGrammar = Just Nothing

{-| keep (as the given constructor), always. -}
keepIsFiniteGrammar :: (t -> FiniteGrammar t) -> (t -> IsFiniteGrammar t)
keepIsFiniteGrammar f = Just . Just . f

{-| enumerate all sentences that the (finite) grammar recognizes.

has duplicates. 

>>> type MyRHS = RHS (ConstName String) String (Const ()) String 
>>> let myNumber = NonTerminal (ConstName "myNumber number") $ terminal "0" <|> terminal "1" :: MyRHS
>>> let myGrammar = (++) <$> myNumber <*> (myNumber-?-"0") :: MyRHS
>>> let Just (Just myFiniteGrammar) = isFiniteGrammar (const ignoreIsFiniteGrammar) myGrammar 
>>> let mySentences = List.nub $ enumerateSentences myFiniteGrammar 
>>> mySentences
[Sentence ["0"],Sentence ["0","0"],Sentence ["0","1"],Sentence ["1"],Sentence ["1","0"],Sentence ["1","1"]]

-}
enumerateSentences :: FiniteGrammar t -> [Sentence t]
enumerateSentences = go
 where 
 -- NOTE each case outputs a nonempty list, as does the whole function. 
 go = \case
  FiniteTerminal t      -> [Sentence [t]]
  FiniteOptional r      -> Sentence [] : go r
  FiniteSequence r1 r2  -> (<>) <$> go r1 <*> go r2
  FiniteAlternatives rs -> (concat . cross) (fmap go rs)

rhsEnumerateSentences :: (forall x. f x -> IsFiniteGrammar t) -> RHS n t f a -> Maybe [Sentence t]
rhsEnumerateSentences fromF = fmap enumerateSentences . join . isFiniteGrammar fromF 

{-| 

ignores @f@. 

no duplicates.

-}
rhsEnumerateSentencesSimply :: (Ord t) => RHS n t f a -> Maybe [Sentence t]
rhsEnumerateSentencesSimply
 = fmap List.nub
 . fmap enumerateSentences
 . join
 . isFiniteGrammar (const ignoreIsFiniteGrammar)


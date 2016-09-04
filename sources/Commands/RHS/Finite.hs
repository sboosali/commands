{-# LANGUAGE ScopedTypeVariables, RankNTypes, LambdaCase, BangPatterns, TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving, OverloadedStrings #-}

{-|

-}
module Commands.RHS.Finite where
import Commands.Extra 
import Commands.RHS.Open -- Types
-- import Commands.RHS.Derived 

-- import Derive.List 

import Data.Semigroup
import Data.List.NonEmpty (NonEmpty) 
-- import qualified Data.List.NonEmpty as NonEmpty 
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy                    as T
import           Text.PrettyPrint.Leijen.Text      hiding ((<>),(<$>))
import Control.Monad.Trans.Either

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import           Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Error.Class
import Data.Functor.Identity

import Data.Maybe
import Data.Function
-- import GHC.Exts (IsList, IsString(..) )
import Data.Void (Void)  
import           Control.Monad


--------------------------------------------------------------------------------

{-| an 'RHS' subset that is finite and homogeneous. 

to be finite: 

* it lacks grammatical repetition. i.e. 'Some'/'Many'.  
* its fields are all (TODO) strict. you shouldn't define FiniteGrammar values recursively. 


-}
type FiniteGrammar t n = NonEmpty (FiniteProduction t n)

-- Abstract
-- LAW n ∈ Map n 
-- newtype FiniteGrammar t n = FiniteGrammar { 
--  rootFiniteGrammar  :: n
--  rulesFiniteGrammar :: Map n (FiniteProduction t n)
-- }

-- getFiniteGrammarRoot :: FiniteGrammar t n -> FiniteProduction t n
-- getFiniteGrammarRoot FiniteGrammar{..} = Map.lookup rootFiniteGrammar rulesFiniteGrammar & maybe __BUG__ id
--  where
--  __BUG__ = error "getFiniteGrammarRoot"

{-| 

-}
data FiniteProduction t n
 = FiniteTerminal !t --TODO
 | FiniteNonTerminal !n
 | FiniteOptional !(FiniteProduction t n)
 | FiniteSequence !(FiniteProduction t n) !(FiniteProduction t n) -- TODO ![FiniteProduction t n] (Nested Pairs is more natural Only for a typed/Heterogeneous sequence)
 | FiniteAlternatives ![FiniteProduction t n] -- TODO strict list import Data.Sequence (Seq) 
 deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable,Data,Generic)

{-| an un-associated 'FiniteGrammar', i.e. without names.

after de-associating, the size of the grammar can explode (analogous with dynamic programming).

abandons any sharing information.

-}
type AnonymousFiniteGrammar t = NonEmpty (AnonymousFiniteProduction t) --TODO?

{-|

-}
type AnonymousFiniteProduction t = FiniteProduction t Void --TODO?

-- deriveSemigroup ''FiniteProduction 'FiniteAlternatives 

{-| state/failure effects.

-}
type IsFiniteGrammar t n =
  StateT (Map n (FiniteProduction t n))
   (EitherT ()
    Identity) ()

{-| a grammatical sentence. 

-}
newtype Sentence t = Sentence [t]
 deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable,Semigroup,Monoid,Data,Generic) -- TODO custom IsList and IsString

unSentence :: Sentence t -> [t] -- avoids polluting Show instance with accessor --TODO getSentence
unSentence (Sentence ts) = ts

toSentence :: String -> Sentence String 
-- toSentence :: (IsString t) => t -> Sentence t
toSentence = Sentence . words

--------------------------------------------------------------------------------

{-TODO
use sharing??
FiniteNonTerminal

-}

-- rhsEnumerateSentences :: (forall x. f x -> IsFiniteGrammar t n) -> RHS n t f a -> Maybe [Sentence t]
-- rhsEnumerateSentences fromF = fmap enumerateSentences . join . isFiniteGrammar fromF 

-- {-| 

-- ignores @f@. 

-- no duplicates.

-- -}
-- rhsEnumerateSentencesSimply :: (Ord t) => RHS n t f a -> Maybe [Sentence t]
-- rhsEnumerateSentencesSimply
--  = fmap ordNub
--  . fmap enumerateSentences
--  . join
--  . isFiniteGrammar (const ignoreIsFiniteGrammar)

--------------------------------------------------------------------------------

-- {-| 

-- -}
-- finitizeRHS0 :: RHS0 t n f -> FiniteGrammar t n
-- finitizeRHS0 = undefined

--------------------------------------------------------------------------------

{- | outputs a grammar that is finite and homogeneous. 

-}
isFiniteGrammar
 :: forall t n f a. ()
 => (forall x.         f x -> IsFiniteGrammar t n)
 -> (          RHS (Const10 n) t f a -> IsFiniteGrammar t n) 

isFiniteGrammar fromF = \case

 Many{} -> abortIsFiniteGrammar
 Some{} -> abortIsFiniteGrammar

 Terminals{} -> ignoreIsFiniteGrammar         -- NOTE being the set of terminals, it should add no new terminals 
 Pure{}      -> ignoreIsFiniteGrammar

 Terminal    _ t -> (keepIsFiniteGrammar FiniteTerminal) t 
 NonTerminal n r -> undefined FiniteNonTerminal n go r
 r `Apply` f     -> (when2_ FiniteSequence)     <$> (go r) <*> (fromF f)
 r1 :<*>   r2    -> (when2_ FiniteSequence)     <$> (go r1) <*> (go r2)
 Alter rs        -> (whenN_ FiniteAlternatives) <$> (traverse go rs)
 Opt  _ r        -> (when1_ FiniteOptional)     <$> (go r)

 where

 go :: forall x. RHS n t f x -> IsFiniteGrammar t n
 go = isFiniteGrammar fromF

 -- keep (as the given constructor), only when the grammar may be finite. 
 when1_ f = fmap f 
 when2_ f = liftA2 f 
 whenN_ f = Just . f . catMaybes

{-| abort (the grammar is infinite) -}
abortIsFiniteGrammar :: IsFiniteGrammar t n
abortIsFiniteGrammar = throwError () -- Left

{-| ignore (the grammar has no subgrammars, and thus is finite) -}
ignoreIsFiniteGrammar :: IsFiniteGrammar t n
ignoreIsFiniteGrammar = return () -- Right

{-| keep (as the given constructor), always. -}
keepIsFiniteGrammar :: (t -> FiniteGrammar t n) -> (t -> IsFiniteGrammar t n)
keepIsFiniteGrammar f = return . f

--------------------------------------------------------------------------------

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
enumerateSentences :: FiniteGrammar t n -> [Sentence t]
enumerateSentences = go
 where 
 -- NOTE each case outputs a nonempty list, as does the whole function. 
 go = \case
  FiniteTerminal t      -> [Sentence [t]]
  FiniteOptional r      -> Sentence [] : go r
  FiniteSequence r1 r2  -> (<>) <$> go r1 <*> go r2
  FiniteAlternatives rs -> (concat . cross) (fmap go rs) --TODO too slow, generates millions On simple Grammar.

--------------------------------------------------------------------------------

{-old

List.nub is n^2

-}

{-| wraps 'ppFiniteGrammar'

-}
displayFiniteGrammar :: FiniteGrammar Text Text -> String
displayFiniteGrammar = ppFiniteGrammar >>> renderOneLine >>> displayT >>> T.unpack

{-| a pretty printer.

uses @"(" ")" "?" "|" "\""@

e.g.

@

@

-}
ppFiniteGrammar :: FiniteGrammar Text Text -> Doc
ppFiniteGrammar = undefined

ppFiniteProduction :: FiniteProduction Text Text -> Doc
ppFiniteProduction = \case
  FiniteTerminal t      -> dquotes (text t) -- the terminals might have spaces; disambiguates from the FiniteSequence case
  FiniteTerminal t      -> dquotes (text t) -- the terminals might have spaces; disambiguates from the FiniteSequence case
  FiniteOptional r      -> mconcat [ "(" , ppFiniteProduction r , ")?" ]
  FiniteSequence r1 r2  -> mconcat [ ppFiniteProduction r1 , " " , ppFiniteProduction r2 ]
  FiniteAlternatives rs -> mconcat [ "(" , rs & (fmap ppFiniteProduction >>> punctuate " | " >>> cat) , ")" ]

{-| >>> displaySentence ["this", "is", "a", "sentence"] 
"this is a sentence" 

-}
displaySentence :: Sentence String -> String
displaySentence = List.intercalate " " . unSentence

printSentence :: Sentence String -> IO ()
printSentence sentence = do 
 putStrLn $ displaySentence sentence 

--------------------------------------------------------------------------------

{-old

displayDoc

-}

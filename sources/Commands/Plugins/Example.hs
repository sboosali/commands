{-# LANGUAGE ExtendedDefaultRules, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms, RankNTypes, ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell, TupleSections                      #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind -fno-warn-orphans -fno-warn-unused-imports -fno-warn-type-defaults #-}
module Commands.Plugins.Example where
import           Commands.Etc                      ()
import           Commands.Frontends.Dragon13
import           Commands.Frontends.Dragon13.Text
import           Commands.Frontends.Dragon13.Types
import           Commands.Parse
-- import Commands.Parse.Types
import           Commands.Grammar
import           Commands.Grammar.Types
import           Control.Alternative.Free.Johansen

-- import Data.Maybe (maybe)
-- import qualified Data.Map as Map
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad                     (void, (<=<), (>=>))
import           Control.Parallel
import           Data.Bitraversable
import           Data.Foldable                     (Foldable (..))
import           Data.List.NonEmpty                (fromList)
import qualified Data.Text.Lazy.IO                 as T
import           Language.Python.Common.AST        (Expr (Dictionary, Strings))
import           Language.Python.Version2.Parser   (parseExpr, parseModule)
import           Prelude                           hiding (foldr)
import           System.Timeout                    (timeout)
import           Text.PrettyPrint.Leijen.Text      hiding (empty, int, (<$>),
                                                    (<>))

-- import Commands.Render

-- import qualified Data.Text.Lazy                    as T
-- import Control.Lens (alongside,Prism',Traversal',Lens)
-- import Control.Exception.Lens (handling, _IOException, AsIOException)
-- import System.IO.Error.Lens (description, location)
-- -- import System.IO.Error.Lens (errorType,_UserError,description)
-- import           Data.Monoid                       ((<>))
-- import           Data.Either.Validation            (Validation (..))
-- import Commands.Parsec
-- import Control.Applicative.Permutation
-- import Data.List                       (intercalate)
-- import Data.Monoid                     ((<>))
-- import Data.Traversable (traverse)



data Root
 = ReplaceWith Dictation Dictation
 | Undo
 | Repeat Positive Root
 deriving (Show,Eq)

root :: Grammar Root
root = 'root <=> empty

 <|> ReplaceWith <$> (terminal "replace" *> inject dictation) <*> (terminal "with" *> inject dictation)
 <|> Undo        <$ terminal "no way"
 <|> Undo        <$ terminal "no"
 <|> Repeat      <$> inject positive <*> inject root

newtype Positive = Positive Int deriving (Show,Eq)
positive :: Grammar Positive
positive = 'positive
 <=> Positive # (foldr (<|>) empty . map int) [1..9]

newtype Dictation = Dictation String deriving (Show,Eq)
dictation :: Grammar Dictation
dictation = 'dictation
 <=> Dictation # str "this"
 <|> Dictation # str "that"

-- dictation = grammar "<dgndictation>" $ \context ->
--  Dictation <$> anyWord `manyUntil` context

-- -- | context-sensitive grammars (like 'dictation') work (?) with 'atom'
-- data Directions = Directions Dictation Dictation Dictation deriving (Show,Eq)
-- directions :: Grammar Directions
-- directions = terminal "directions" *> (runPerms $ Directions
--  <$> atom (terminal "from" *> dictation)
--  <*> atom (terminal "to"   *> dictation)
--  <*> atom (terminal "by"   *> dictation))


-- data Place = Place String deriving (Show,Eq)
-- place :: Grammar Place
-- place = Place <$> freely "<Place>" anyWord

-- data Transport = Foot | Bike | Bus | Car deriving (Show,Eq,Enum)
-- transport :: Grammar Transport
-- transport = twig

-- -- | context-free grammars (like from 'twig' or 'anyWord') can use 'maybeAtom'
-- data DirectionsF = DirectionsF (Maybe Place) (Maybe Place) (Maybe Transport) deriving (Show,Eq)
-- directions' :: Grammar DirectionsF
-- directions' = terminal "directions" *> (runPerms $ DirectionsF
--  <$> maybeAtom (terminal "from" *> place)
--  <*> maybeAtom (terminal "to"   *> place)
--  <*> maybeAtom (terminal "by"   *> transport))


-- exampleDirections =
--  [ "directions from here to there  by bike"
--  , "directions from here by bike   to there"
--  , "directions to there  from here by bike"
--  , "directions to there  by bike   from here"
--  , "directions by bike   from here to there"
--  , "directions by bike   to there  from here"
--  ]

-- goodDirections  = Directions  (Dictation ["here"])  (Dictation ["there"])  (Dictation ["bike"])
-- goodDirectionsF = DirectionsF (Just (Place "here")) (Just (Place "there")) (Just Bike)

-- powerset :: [a] -> [[a]]
-- powerset [] = [[]]
-- powerset (x:xs) = xss <> map (x:) xss
--  where xss = powerset xs

-- inputDirectionsF  = map (intercalate " ") . map ("directions":) . powerset $ ["by bike","to there","from here"]
-- outputDirectionsF =
--  [ DirectionsF Nothing               Nothing                Nothing
--  , DirectionsF (Just (Place "here")) Nothing                Nothing
--  , DirectionsF Nothing               (Just (Place "there")) Nothing
--  , DirectionsF (Just (Place "here")) (Just (Place "there")) Nothing
--  , DirectionsF Nothing               Nothing                (Just Bike)
--  , DirectionsF (Just (Place "here")) Nothing                (Just Bike)
--  , DirectionsF Nothing               (Just (Place "there")) (Just Bike)
--  , DirectionsF (Just (Place "here")) (Just (Place "there")) (Just Bike)
--  ]

grammar = DNSGrammar export [command, subcommand, flag] :: DNSGrammar Text Text

export = DNSProduction (DNSRule "export") $ fromList
 [ DNSSequence $ fromList
   [ DNSNonTerminal (DNSList "command")
   , DNSNonTerminal (DNSRule "subcommand")
   , DNSOptional (DNSMultiple (DNSNonTerminal (DNSList "flag")))
   ]
 , DNSTerminal (DNSToken "ls")
 ]

command = DNSVocabulary (DNSList "command")
 [ DNSToken "git"
 , DNSToken "rm"
 ]

subcommand = DNSProduction (DNSRule "subcommand") $ fromList
 [ DNSTerminal (DNSToken "status")
 , DNSNonTerminal (DNSBuiltin DGNDictation)
 ]

flag = DNSVocabulary (DNSList "flag")
 [ DNSPronounced "-f" "force"
 , DNSPronounced "-r" "recursive"
 , DNSPronounced "-a" "all"
 , DNSPronounced "-i" "interactive"
 ]

-- | traverse with monoidal error collecting
badGrammar = DNSGrammar (DNSProduction (DNSRule "bad root") $ fromList [DNSTerminal (DNSToken "'")]) [] :: DNSGrammar Text Text


isPythonDict :: String -> Bool
isPythonDict s = case parseExpr s "" of
 Right (Dictionary {}, _) -> True
 _ -> False

isPythonString :: String -> Bool
isPythonString s = case parseExpr s "" of
 Right (Strings {}, _) -> True
 _ -> False

isPythonModule :: String -> Bool
isPythonModule s = case parseModule s "" of
 Right {} -> True
 _ -> False

oneSecond :: Int
oneSecond = round (1e6 :: Double)

-- attempt :: IO () -> IO ()
-- attempt = maybe (putStrLn ".") (const (return ())) <=< timeout (round (1e6 :: Double))
-- attempt action = do
--  handle <- async $ timeout oneSecond action
--  waitCatch handle >>= \case
--   Left error     -> print error
--   Right Nothing  -> putStrLn "."
--   Right (Just _) -> return ()
-- attempt action = do
--  timeout oneSecond action `withAsync` \handle -> do
--   waitCatch handle >>= \case
--    Left error     -> print error
--    Right Nothing  -> putStrLn "."
--    Right (Just _) -> return ()
-- attempt action = do
--  forkIO $ timeout oneSecond action >>= \case
--   Nothing  -> putStrLn "."
--   (Just _) -> return ()


-- attemptForking action = do
--  forkFinally (timeout oneSecond action) $ \case
--    Left error     -> print error
--    Right Nothing  -> putStrLn "."
--    Right (Just _) -> print ()
attemptForking action = do
 forkFinally (timeout oneSecond action) $ \case
   Left error     -> print error
   Right Nothing  -> putStrLn "."
   Right (Just _) -> print ()

attemptAsynchronously action = do
 (timeout oneSecond action) `withAsync` (waitCatch >=> \case
   Left error     -> print error
   Right Nothing  -> putStrLn "."
   Right (Just _) -> return ()
  )

attempt = attemptAsynchronously

-- attempting :: [IO ()] -> IO ()
-- attempting actions = do
--  sequence_ actions
attempting :: [IO ()] -> IO ()
attempting actions = do
 sequence_ actions

attemptParse p s = attempt (print =<< (p `parses` s))


main = do
 -- let Right escaped = escapeDNSGrammar grammar
 -- let serializedRules = vsep ["'''", serializeRules escaped, "'''"]
 -- let serializedLists = serializeVocabularies (dnsProductions escaped)
 -- let serializedGrammar = vsep $ punctuate "\n" [ "_commands_rules_ =" <+> serializedRules, "_commands_lists_ =" <+> serializedLists]
 -- print serializedGrammar

 -- putStrLn ""
 -- putStr "words: "
 -- print $ getWords grammar
 -- putStr "names: "
 -- print $ getNames grammar

 -- putStrLn ""
 -- putStrLn . take 100 . show $ parseExpr (show serializedRules) ""
 -- putStrLn ""
 -- putStrLn . take 100 . show $ parseExpr (show serializedLists) ""
 -- putStrLn ""
 -- putStrLn . take 100 . show $ parseModule (show serializedGrammar) ""

 -- putStrLn ""
 -- print $ (isPythonString . show) serializedRules
 -- print $ (isPythonDict . show) serializedLists
 -- print $ (isPythonModule . show) serializedGrammar

 -- putStrLn ""
 -- _ <- bitraverse print T.putStrLn $ serialize grammar
 -- putStrLn ""
 -- _ <- bitraverse print T.putStrLn $ serialize badGrammar
 -- putStrLn ""

 -- print escaped


 putStrLn ""
 print $ fromLeaves [(+1), (*10)] <*> fromLeaves [1,2,3]
 -- Branch [Branch [Leaf 2,Leaf 3,Leaf 4],Branch [Leaf 10,Leaf 20,Leaf 30]]
 print $ (+) <$> fromLeaves [1, 10] <*> fromLeaves [1,2,3]
 -- Branch [Branch [Leaf 2,Leaf 3,Leaf 4],Branch [Leaf 11,Leaf 12,Leaf 13]]
 print $ (,,) <$> fromLeaves [0] <*> fromLeaves [False, True] <*> fromLeaves ['a', 'b', 'c']
 -- Branch
 --  [Branch                                                             -- the 0 branch
 --   [Branch [Leaf (0,False,'a'),Leaf (0,False,'b'),Leaf (0,False,'c')] -- the False branch
 --   ,Branch [Leaf (0,True,'a'),Leaf (0,True,'b'),Leaf (0,True,'c')]]]  -- the True branch
 print $ (,,) <$> fromLeaves [False, True] <*> fromLeaves ['a', 'b', 'c'] <*> fromLeaves [1,2,3]
{-
Branch
 [Branch                                                               -- the False branch
  [ Branch [Leaf (False,'a',1),Leaf (False,'a',2),Leaf (False,'a',3)]  -- the False 'a' branch
  , Branch [Leaf (False,'b',1),Leaf (False,'b',2),Leaf (False,'b',3)]  -- the False 'b' branch
  , Branch [Leaf (False,'c',1),Leaf (False,'c',2),Leaf (False,'c',3)]] -- the False 'c' branch
 , Branch                                                            -- the True branch
  [ Branch [Leaf (True,'a',1),Leaf (True,'a',2),Leaf (True,'a',3)]   -- the True 'a'  branch
  , Branch [Leaf (True,'b',1),Leaf (True,'b',2),Leaf (True,'b',3)]   -- the True 'b' branch
  , Branch [Leaf (True,'c',1),Leaf (True,'c',2),Leaf (True,'c',3)]]] -- the True 'c' branch
-}
 attempt $ print $ foldr (&&) True $ Branch [Leaf False, fromLeaves [False,False ..]]


 putStrLn ""
 handleParse positive "9"
 handleParse dictation "that"

 -- attempting
 --  [ handleParse root "no"
 --  , handleParse root "no way"
 --  , handleParse root "replace this with that"
 --  , handleParse root "1 no"
 --  , handleParse root "1 1 no"
 --  ]

 attemptParse root "no"
 -- attemptParse root "no way"
 -- attemptParse root "replace this with that"
 -- attemptParse root "1 no"
 attemptParse root "1 1 no"

 putStrLn ""
 -- attempt (print $ counts root)
 -- attempt $ print dictation
 -- attempt $ print $ Map.keys $ reifyGrammar positive
 -- attempt $ print $ Map.keys $ reifyGrammar dictation
 -- timeout (round (1e2 :: Double)) $ print $ Map.keys $ reifyGrammar root

 -- attempt $ print $ length $ alternatives $ inject positive -- should be one "lexically", but is nine "semantically"
 -- attempt $ print $ length $ alternatives $ inject dictation -- is two
 -- attempt $ print $ length $ alternatives $ inject root -- should be three, but it's infinity

 print $ fmap (const ()) $ alternatives $ inject positive -- it's flat??
 print $ fmap (const ()) $ alternatives $ inject dictation
 -- print $ fmap (const ()) $ alternatives $ inject root

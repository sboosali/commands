{-# LANGUAGE FlexibleContexts, LambdaCase, RankNTypes, TypeSynonymInstances, FlexibleInstances, ViewPatterns, OverloadedStrings, ScopedTypeVariables, ConstraintKinds, NamedFieldPuns, NoMonomorphismRestriction   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures -fno-warn-orphans #-}
module Commands.Plugins.Spiros.Extra
 ( module Commands.Plugins.Spiros.Extra
 , module X

 , module Commands.Plugins.Spiros.Extra.Types
 , module Commands.Plugins.Spiros.Rank

 , module Commands.Extra
-- , module Prelude.Spiros

 ) where

import           Commands.Extra hiding (insert) -- the module is a reexport
-- Workflow.insert is used by the `.Run` modules

import Commands.Plugins.Spiros.Types
import Commands.Plugins.Spiros.Extra.Types
import Commands.Plugins.Spiros.Rank

import           Commands.Mixins.DNS13OSX9
import           Commands.Backends.Workflow

import qualified System.FilePath.Posix as FilePath
import System.Clock (TimeSpec,toNanoSecs,diffTimeSpec)
import Language.Python.Common.Token
import Language.Python.Common.SrcLocation
import Language.Python.Common.ParseError
import qualified Data.Text.Lazy                as T
import Data.Text.Lazy (Text)
-- import           Control.Lens(imap)
import Data.Semigroup ((<>))
import Data.Default as X

import Data.Foldable
import qualified Data.List as List
import System.Exit(ExitCode(..))
import           GHC.Exts                        (IsString)
import Text.Printf (printf)
import System.IO
import System.Process
import Data.Function as X
import Control.Monad  (replicateM_) 

--import Prelude.Spiros -- already reexported by commands.extra
import Prelude(toEnum)

type Desugaring a = a -> SpirosMonad_ -- TODO

-- ================================================================ --

whenJust :: (Monad m) => Maybe a -> (m () -> m ())
whenJust condition_ action_ = ifJust condition_ action_ nothing

ifJust :: (Monad m) => Maybe b -> m a -> m a -> m a
ifJust condition_ actionTrue_ actionFalse_ = maybe (actionFalse_) (const actionTrue_) condition_

-- ================================================================ --

-- ================================================================ --
-- debugging

-- import           Commands.Frontends.Dragon13
-- import           Commands.Plugins.Example.Spacing

-- import           Control.Lens hiding (from, ( # ))
-- import           Data.List.NonEmpty (NonEmpty (..))
-- import           Data.List.NonEmpty (NonEmpty (..))
-- import qualified Data.Text.Lazy as T
-- import qualified Data.Text.Lazy.IO as T

-- import           Control.Concurrent.Async
-- import           System.Timeout (timeout)

-- -- it seems to be synchronous, even with threaded I guess?
-- attemptAsynchronously :: Int -> IO () -> IO ()
-- attemptAsynchronously seconds action = do
--  (timeout (seconds * round (1e6::Double)) action) `withAsync` (waitCatch >=> \case
--    Left error     -> print error
--    Right Nothing  -> putStrLn "..."
--    Right (Just _) -> return ()
--   )

-- attempt = attemptAsynchronously 1

-- -- pseudo HTML ordered list
-- ol xs = ifor_ xs $ \i x -> do
--  putStrLn ""
--  putStrLn $ fold [show i, ". ", x]

-- attemptParse :: (Show a) => (forall  z. DNSEarleyRHS z a) -> String -> IO ()
-- attemptParse rule s = do
--  putStrLn ""
--  attempt $ parseThrow rule ((T.words . T.pack) s) >>= \case
--   x :| _ -> print x

-- attemptSerialize rhs = attemptAsynchronously 3 $ do
--  serialized <- formatRHS rhs
--  either print printSerializedGrammar serialized

-- printSerializedGrammar SerializedGrammar{..} = do
--  replicateM_ 3 $ putStrLn ""
--  T.putStrLn $ displayDoc serializedRules
--  putStrLn ""
--  T.putStrLn $ displayDoc serializedLists

-- main = do
--  putStrLn ""
--  let rootG = root
--  attemptSerialize rootG
--  attemptMunge "par round grave camel lit with async break break action"

-- ================================================================ --

{- | returns the digits that make up a number in some base, most-significant digit first.

>>> toDigits 10 9
[9]

>>> toDigits 10 1234
[1,2,3,4]

assumes nonnegative input, but still is total.

prop> \(Positive k) n -> ('fromDigits' k . toDigits k) n == n

-}
toDigits :: (Integral a) => a -> a -> [a]
toDigits base = reverse . List.unfoldr go
 where
 go n = if n `lessThan` 1 then Nothing else Just ((n `mod` base), (n `div` base))

{- | returns the digits that make up a number in some base, most-significant digit first.

>>> fromDigits 10 [1,2,3,4]
1234

>>> fromDigits 10 [9]
9

>>> fromDigits 10 []
0

-}
fromDigits :: forall a. (Integral a) => a -> [a] -> a
fromDigits base = foldr (+) 0 . zipWith (\i n -> (base^i) * n) [(0::a)..] . reverse

-- TODO remove fromDecimalDigits = fromDigits 10

-- average xs = realToFrac (sum xs) / genericLength xs
safeAverage :: Foldable t => t Int -> Int
safeAverage (toList -> []) = 0
safeAverage xs = sum xs `div` length xs

pause = do
  delay 30

-- slot :: MonadWorkflow m => String -> m ()
slot s = do
 delay 10
 sendText s
 press "<ret>"

isDefaultBrowser :: MonadWorkflow m => m (Maybe String)
isDefaultBrowser = currentApplication >>= \case
 x@"Google Chrome" -> return$ Just x
 _                 -> return$ Nothing

isBrowser x = if FilePath.takeBaseName x `elem` ["Firefox", "Chrome"]
 then Just x
 else Nothing

defaultDelay = 100 :: Int

chromeDelay = 250 :: Int                 -- milliseconds
 -- in chrome, keypresses are lost when the delay isnt long enough

browserDelay = chromeDelay

-- runRepeat :: (MonadWorkflow m) => Int -> Number -> (m () -> m ())
runRepeat delay_ times_
 = traverse_ id
 . List.intersperse (delay delay_)
 . replicate times_
 --TODO action grouping: insert nullop between each, for logging

-- used when sendText is too slow/laggy
-- insertByClipboard :: String -> AMonadAction_
-- insertByClipboard :: MonadWorkflow m => String -> m ()
insertByClipboard s = do
 setClipboard s
 presspaste

-- presspaste :: MonadWorkflow m => m ()
presspaste = press "M-v"

-- runs the action, then restores the previous clipboard contents. dictation still pollutes clipboard history, but the most recent "manual" clipboard contents should be preserved.
-- benign race condition, as no lock is kept on the system clipboard
restoringClipboard :: Workflow a -> Workflow a
restoringClipboard m = do
 contents <- getClipboard
 x <- m
 delay 100 -- otherwise, e.g. the old clipboard contents are reset before the temporary clipboard contents are paste TODO call Haskell from Objective-C on callback?
 setClipboard contents
 return x

diffTimeSpecAsMilliseconds :: TimeSpec -> TimeSpec -> Integer
diffTimeSpecAsMilliseconds x y = (toNanoSecs (diffTimeSpec x y)) `div` (1000*1000)

bool2exitcode :: Bool -> ExitCode
bool2exitcode False = ExitFailure 1
bool2exitcode True  = ExitSuccess

padNumber :: Integral a => Int -> a -> String
padNumber padding n = printf ("%0." ++ show padding ++ "d") (toInteger n)

leftAppendLineNumbers :: Text -> (Int,Int,Text)
leftAppendLineNumbers code = (marginWidth, countWidth, (T.unlines . _imap go) allLines)
 where
 go ((+1) -> lineNumber) oneLine = getLeftMargin lineNumber <> oneLine
 marginWidth = (fromInteger . toInteger . T.length) (getLeftMargin (0::Integer))  -- assumes the length is constant
 getLeftMargin lineNumber = "[" <> T.pack (padNumber countWidth lineNumber) <> "]"
 countWidth = length (show lineCount)
 lineCount = length allLines
 allLines = T.lines code
 _imap :: (Integer -> a -> b) -> [a] -> [b] 
 _imap f xs = zipWith f [1..] xs

getPythonErrorSpan :: ParseError -> (Int,Int)
getPythonErrorSpan = maybe (1,1) id . go -- TODO default error span
 where

 go = \case
  UnexpectedToken (token_span -> theSpan) -> fromSourceSpan theSpan
  UnexpectedChar _ location -> fromSourceLocation location
  _ -> Nothing

 fromSourceSpan = \case
  SpanCoLinear{ span_row, span_start_column } -> Just (span_row, span_start_column)
  SpanMultiLine{ span_start_row, span_start_column } -> Just (span_start_row, span_start_column)
  SpanPoint{ span_row, span_column } -> Just (span_row, span_column)
  _ -> Nothing

 fromSourceLocation = \case
  Sloc{ sloc_row, sloc_column } -> Just (sloc_row, sloc_column)
  _ -> Nothing

showWords :: [Text] -> String
showWords = T.unpack . T.intercalate (T.pack " ")

printMessage :: [String] -> IO ()
printMessage = putStrLn . List.intercalate "\n"

prompt :: String -> IO String
prompt s = do
    putStr s
    hFlush stdout
    getLine

type Vocab a = [(String, a)]

vocabWith :: (IsString t, Show t, Functor'RHS n t f) => (a->b) -> Vocab a -> RHS n t f b
vocabWith f = vocab . fmap (fmap f)

{-| inputs milliseconds, outputs microseconds (which can be given to threadDelay).

>>> milliseconds 10
10000

-}
milliseconds :: Int -> Int
milliseconds = (*1000)

{-| inputs seconds, outputs microseconds (which can be given to threadDelay).

>>> seconds 1
1000000

-}
seconds :: Int -> Int
seconds = (*1000000)

{-|

-}
readCommand aCommand someArguments = do
 (exitCode, standardInput, standardError) <- readProcessWithExitCode aCommand someArguments ""
 return (exitCode, lines standardInput, lines standardError)

readSpirosContext :: String -> SpirosContext
readSpirosContext = \case
 (isEmacsApp -> Just{}) -> EmacsContext
 "Google Chrome" -> ChromeContext
 "IntelliJ" -> IntelliJContext
 _ -> GlobalContext

isEmacsApp :: FilePath -> Maybe FilePath
isEmacsApp fp = if FilePath.takeBaseName fp `elem` ["Emacs","Work","Notes","Diary","Obs","Commands"]
 then Just fp
 else Nothing

filterMempty :: (Monoid a, Eq a) => [a] -> [a]
filterMempty = filter (/= mempty)

{-| 'intersperse's a 'delay' between each action.

e.g.

@
withDelay 30                 -- milliseconds
 [ press "H-<tab>"
 , insert =<< getClipboard
 , press "H-<tab>"
 ]
@

-}
withDelay :: (MonadWorkflow m) => Int -> [m ()] -> m ()
withDelay t = sequence_ . List.intersperse (delay t)

-- | The constructors of a (zero-based) Enum.
--
-- >>> constructors :: [Bool]
-- [False,True]
--
-- (Bounded Constraint elided for convenience; doesn't terminate on un@Bounded@ @Enum@erations)
--
constructors :: (Enum a) => [a]
constructors = enumFrom (toEnum 0)

-- ================================================================ --
-- comapt

--TODO -- | my runWorkflow
-- runWorkflow' :: WorkflowT IO a -> IO a
-- runWorkflow' = runWorkflowT def

{- old

delaying

-}

replicateDelayingA :: (Applicative m) => Natural -> m a -> m ()
replicateDelayingA k m = replicateM_ (k&integral) m

integral :: (Integral i, Num j) => i -> j
integral = toInteger > fromInteger

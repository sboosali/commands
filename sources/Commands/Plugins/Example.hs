{-# LANGUAGE DeriveDataTypeable, ExtendedDefaultRules, LambdaCase #-}
{-# LANGUAGE NamedFieldPuns, PatternSynonyms, RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TupleSections  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind -fno-warn-orphans -fno-warn-unused-imports -fno-warn-type-defaults #-}
module Commands.Plugins.Example where
import           Commands.Command
import           Commands.Command.Combinator
import           Commands.Command.Sugar
import           Commands.Command.Types             ()
import           Commands.Etc                       ()
import           Commands.Frontends.Dragon13
import           Commands.Frontends.Dragon13.Render
import           Commands.Frontends.Dragon13.Text
import           Commands.Frontends.Dragon13.Types
import           Commands.Grammar
import           Commands.Grammar.Types
import           Commands.Parse
import           Commands.Parse.Types
import           Commands.Parsec
import           Control.Alternative.Free.Tree

import           Control.Applicative
import           Control.Applicative.Permutation
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad                      (void, (<=<), (>=>))
import           Control.Monad.Catch                (catches)
import           Control.Parallel
import           Data.Bitraversable
import           Data.Either                        (either)
import           Data.Foldable                      (Foldable (..), asum,
                                                     traverse_)
import           Data.List.NonEmpty                 (NonEmpty (..), fromList)
import qualified Data.Text.Lazy.IO                  as T
import           Data.Typeable
import           Language.Python.Common.AST         (Expr (Dictionary, Strings))
import           Language.Python.Version2.Parser    (parseExpr, parseModule)
import           Prelude                            hiding (foldr)
import           System.Timeout                     (timeout)
import           Text.PrettyPrint.Leijen.Text       hiding (empty, int, (<$>),
                                                     (<>))

-- import qualified Data.Text.Lazy                    as T
-- import Control.Lens (alongside,Prism',Traversal',Lens)
-- import Control.Exception.Lens (handling, _IOException, AsIOException)
-- import System.IO.Error.Lens (description, location)
-- -- import System.IO.Error.Lens (errorType,_UserError,description)
-- import           Data.Monoid                       ((<>))
-- import           Data.Either.Validation            (Validation (..))
-- import Control.Applicative.Permutation
-- import Data.List                       (intercalate)
-- import Data.Monoid                     ((<>))
-- import Data.Traversable (traverse)


data Root
 = ReplaceWith Dictation Dictation
 | Undo
 | Repeat Positive Root
 | Dictated Dictation
 | Click_ Click
 -- Roots [Root]
 deriving (Show,Eq)

root :: Command Root
root = 'root <=> empty
 <|> ReplaceWith # "replace" & dictation & "with" & dictation
 <|> Undo        # "no"         -- order matters..
 <|> Undo        # "no way"     -- .. the superstring "no way" should come before the substring "no" (unlike this example)
 <|> Repeat      # positive & root
 <|> Dictated    # "say" & dictation
 <|> Click_      # click
 -- <|> Roots       # (multipleC root)

data Click = Click Times Button deriving (Show,Eq)
click :: Command Click
click = 'click
 <=>  Click # (optionC Single) times & (optionC LeftButton) button & "click"

data Times = Single | Double | Triple deriving (Show,Eq,Enum,Typeable)
times = defaultCommand :: Command Times

data Button = LeftButton | MiddleButton | RightButton deriving (Show,Eq,Typeable)
button :: Command Button
button = 'button
 <=> LeftButton   # "left"
 <|> MiddleButton # "middle"
 <|> RightButton  # "right"
 -- TODO qualified enumeration qualifiedC. defaultC magically detects prefix or suffix qualification and elides the infix with qualifiedC? otherwise enumeratedC

newtype Positive = Positive Int deriving (Show,Eq)
positive :: Command Positive
positive = 'positive
 <=> Positive # (asum . fmap int) [1..9]

newtype Dictation = Dictation [String] deriving (Show,Eq)
dictation :: Command Dictation
dictation = specialCommand 'dictation
 (DNSGrammar (DNSProduction (DNSRule "dictation") (DNSNonTerminal (DNSBuiltin DGNDictation) :| [])) [])
 (\context -> Dictation <$> anyBlack `manyUntil` context)


-- -- | context-sensitive grammars (like 'dictation') work (?) with 'atom'
-- data Directions = Directions Dictation Dictation Dictation deriving (Show,Eq)
-- directions :: Command Directions
-- directions = 'directions <=> liftString "directions" &> (runPerms $ Directions
--  <$> atom (liftString "from" &> liftCommand dictation)
--  <&> atom (liftString "to"   &> liftCommand dictation)
--  <&> atom (liftString "by"   &> liftCommand dictation))

-- -- | context-free grammars (like from 'twig' or 'anyWord') can use 'rhsMaybe'
-- data Directions_ = Directions_ (Maybe Place) (Maybe Place) (Maybe Transport) deriving (Show,Eq)
-- directions_ :: Command Directions_
-- directions_ = 'directions_ <=> liftString "directions" &> (runPerms $ Directions_
--  <$> rhsMaybe (liftString "from" &> liftCommand place)
--  <&> rhsMaybe (liftString "to"   &> liftCommand place)
--  <&> rhsMaybe (liftString "by"   &> liftCommand transport))

-- data Directions__ = Directions__ (Maybe Dictation) (Maybe Dictation) (Maybe Dictation) deriving (Show,Eq)
-- directions__ :: Command Directions__
-- directions__ = 'directions__ <=> terminal "directions" &> (runPerms $ Directions__
--  <$> rhsMaybe (terminal "from" &> liftCommand dictation)
--  <&> rhsMaybe (terminal "to"   &> liftCommand dictation)
--  <&> rhsMaybe (terminal "by"   &> liftCommand dictation))


newtype Place = Place String deriving (Show,Eq)
place = 'place <=> Place # alias ["this","that","bike","here","there"]

data Transport = Foot | Bike | Bus | Car | PublicTransit deriving (Show,Ord,Eq,Enum,Typeable)
transport = defaultCommand

exampleDirections =
 [ "directions from here to there  by bike"
 , "directions from here by bike   to there"
 , "directions to there  from here by bike"
 , "directions to there  by bike   from here"
 , "directions by bike   from here to there"
 , "directions by bike   to there  from here"
 ]

-- goodDirections  = Directions  (Dictation ["here"])  (Dictation ["there"])  (Dictation ["bike"])
-- goodDirections_ = Directions_ (Just (Place "here")) (Just (Place "there")) (Just Bike)

-- powerset :: [a] -> [[a]]
-- powerset [] = [[]]
-- powerset (x:xs) = xss <> fmap (x:) xss
--  where xss = powerset xs

-- inputDirections_  = fmap (intercalate " ") . fmap ("directions":) . powerset $ ["by bike","to there","from here"]
-- outputDirections_ =
--  [ Directions_ Nothing               Nothing                Nothing
--  , Directions_ (Just (Place "here")) Nothing                Nothing
--  , Directions_ Nothing               (Just (Place "there")) Nothing
--  , Directions_ (Just (Place "here")) (Just (Place "there")) Nothing
--  , Directions_ Nothing               Nothing                (Just Bike)
--  , Directions_ (Just (Place "here")) Nothing                (Just Bike)
--  , Directions_ Nothing               (Just (Place "there")) (Just Bike)
--  , Directions_ (Just (Place "here")) (Just (Place "there")) (Just Bike)
--  ]



-- grammar = DNSGrammar export [command, subcommand, flag] :: DNSGrammar Text Text

-- export = DNSProduction (DNSRule "export") $ fromList
--  [ DNSSequence $ fromList
--    [ DNSNonTerminal (DNSList "command")
--    , DNSNonTerminal (DNSRule "subcommand")
--    , DNSOptional (DNSMultiple (DNSNonTerminal (DNSList "flag")))
--    ]
--  , DNSTerminal (DNSToken "ls")
--  ]

-- command = DNSVocabulary (DNSList "command")
--  [ DNSToken "git"
--  , DNSToken "rm"
--  ]

-- subcommand = DNSProduction (DNSRule "subcommand") $ fromList
--  [ DNSTerminal (DNSToken "status")
--  , DNSNonTerminal (DNSBuiltin DGNDictation)
--  ]

-- flag = DNSVocabulary (DNSList "flag")
--  [ DNSPronounced "-f" "force"
--  , DNSPronounced "-r" "recursive"
--  , DNSPronounced "-a" "all"
--  , DNSPronounced "-i" "interactive"
--  ]

-- -- | traverse with monoidal error collecting
-- badGrammar = DNSGrammar (DNSProduction (DNSRule "bad root") $ fromList [DNSTerminal (DNSToken "'")]) [] :: DNSGrammar Text Text


-- isPythonDict :: String -> Bool
-- isPythonDict s = case parseExpr s "" of
--  Right (Dictionary {}, _) -> True
--  _ -> False

-- isPythonString :: String -> Bool
-- isPythonString s = case parseExpr s "" of
--  Right (Strings {}, _) -> True
--  _ -> False

-- isPythonModule :: String -> Bool
-- isPythonModule s = case parseModule s "" of
--  Right {} -> True
--  _ -> False

oneSecond :: Int
oneSecond = round (1e6 :: Double)

attemptAsynchronously action = do
 (timeout oneSecond action) `withAsync` (waitCatch >=> \case
   Left error     -> print error
   Right Nothing  -> putStrLn "."
   Right (Just _) -> return ()
  )


attempt = attemptAsynchronously

attemptParse command = attempt . handleParse command

-- attemptSerialize (Command _ g _) = attempt $ either print print $ (serializeProduction (renderProduction g))
attemptSerialize command = attempt $ either print T.putStrLn $ serialized command


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

 -- putStrLn ""
 -- attempt . print . renders $ root
 putStrLn ""
 attemptSerialize root -- timed out. fast after Commands.Commands.Sugar, I think. Theory: may be left associated tree is efficient, wall arbitrarily associated free alternatives is obscenely polynomial inefficient. But even for such a small grammar? May be non-left association causes non-termination?
 -- I don't think so: {<|> Repeat     <$> liftCommand positive <*> liftCommand root} still terminates in both the serialization in the parsing. Maybe because all the alternatives (or their children) were not left associated? I don't know
 -- See also: attemptParse (multipleC root) "no 1 replace this and that with that and this"

 putStrLn ""
 attemptParse positive "9"
 attemptParse dictation "this and that"
 attemptParse root "no"
 attemptParse root "no way"
 attemptParse root "replace this and that with that and this"
 attemptParse root "1 1 no"
 attemptParse root "say 638 Pine St., Redwood City 94063"
 attemptParse root "no BAD"     -- prefix succeeds, but the whole should fail

 attemptParse (multipleC root) "no no"
 attemptParse (multipleC root) "no replace this with that"
 attemptParse (multipleC root) "no 1 replace this with that"
 attemptParse (multipleC root) "no 1 replace this and that with that and this" -- timed out. left recursion?

 putStrLn ""
 attemptParse click "single left click"
 attemptParse click "left click"
 attemptParse click "single click"
 attemptParse click "click"

 -- putStrLn ""
 -- traverse_ (attemptParse directions_) exampleDirections

 -- putStrLn ""
 -- traverse_ (attemptParse directions) exampleDirections
 -- attemptParse directions "directions from Redwood City to San Francisco by public transit"

 -- putStrLn ""
 -- traverse_ (attemptParse directions__) exampleDirections

 -- putStrLn ""
 -- attemptParse directions__ "directions to San Francisco by public transit from Redwood City"

 -- putStrLn ""
 -- attemptSerialize directions_
 -- putStrLn ""
 -- attemptSerialize root


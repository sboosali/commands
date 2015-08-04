-- {-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults -fno-warn-partial-type-signatures #-}
-- {-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
-- {-# LANGUAGE PostfixOperators, AutoDeriveTypeable, DeriveDataTypeable #-}
-- {-# LANGUAGE ExtendedDefaultRules, TemplateHaskell, RankNTypes, LambdaCase #-}
module Commands.Plugins.Example.Emacs where
-- import           Commands.Etc
-- import           Commands.RHS.Types
-- import           Commands.Plugins.Example.Phrase (Phrase_ (..))
-- import qualified Commands.Plugins.Example.Phrase as P
-- import           Commands.Plugins.Example.Spacing
-- import Commands.Frontends.Dragon13
-- import Data.Sexp
-- import qualified Commands.Backends.OSX as OSX
-- import Commands.Mixins.DNS13OSX9

-- import           Data.List.NonEmpty              (NonEmpty (..))
-- import qualified Data.List.NonEmpty              as NonEmpty
-- import qualified Data.Text.Lazy.IO as T
-- import Control.Lens hiding (snoc, (#))

-- -- {-# LANGUAGE OverloadedLists, OverloadedStrings #-}
-- -- default (Text) -- TODO Necessary? Sufficient?


-- -- ================================================================ --

-- t = T.pack

-- mainEmacs = do
--  print$ parseString edits ("kill")
--  print$ parseString edits ("kill whole word")
--  -- [Edit Cut Forwards Line :| [Edit Select Whole Word_]  "kill" and "_ whole word"
--  -- ,Edit Cut Whole That :| [Edit Select Whole Word_]  "kill _ _" and "_ whole word"
--  -- ,Edit Cut Whole That :| [Edit Select Whole Word_]  ?
--  -- ,Edit Cut Whole Word_ :| []
--  -- ,Edit Cut Whole Word_ :| []
--  -- ]                                          --
--  print$ parseString edit ("kill")            -- [Edit Cut Forwards Line,Edit Cut Whole That] the correct order
--  print$ parseString edit ("kill whole word") -- [Edit Cut Whole Word_,Edit Cut Whole Word_] duplicate

--  print$ parsePhrase_ (T.words$ t"par round grave camel lit with async do break break action")
--  -- "(`async`action)"
--  print$ length $ parseString phrase_ ("par round grave camel lit async break break action")

--  -- loop print$ parseString phrase "par round grave camel lit async break break action"
--  -- print$ parseString root "replace par round grave camel lit with async break break action with blank"
--  -- _phrase <- unsafeSTToIO (renameRHSToEarley >>= ($ phrase))
--  putStrLn ""
--  T.putStrLn =<< showRHS phrase_

--  print $ exampleSexpBlock


-- -- ================================================================ --


-- -- edits :: DNSEarleyRHS r (NonEmpty Edit)
-- -- edits :: RHS (ConstName String) String (forall r. (EarleyF r (NonEmpty Edit) String String [])) (NonEmpty Edit)
-- -- edits :: RHS (ConstName String) String (EarleyF r a String String f) (NonEmpty Edit)
-- -- edits :: RHS (ConstName String) String [] (NonEmpty Edit)
-- -- edits :: RHS (ConstName String) String _ (NonEmpty Edit)
-- -- edits :: Functor f => RHS (ConstName String) String f (NonEmpty Edit)
-- edits = 'edits <=> (edit-+)

-- data Edit = Edit Action Slice Region deriving (Show,Eq,Ord)
-- -- edit :: DNSEarleyRHS r Edit
-- edit = 'edit <=> empty
--  <|> Edit Cut Forwards Line <#> "kill"
--  <|> Edit <#> action            # (slice-?-Whole) # (region-?-That)
--  <|> Edit <#> (action-?-Select) # (slice-?-Whole) # region

-- data Action
--  = Select                       -- read-only.
--  | Copy                         -- read-only.
--  | Cut                          -- read/write.
--  | Delete                       -- read/write.
--  | Transpose                    -- read/write.
--  | Google                       -- read-only.
--  deriving (Show,Eq,Ord)
-- -- action :: DNSEarleyRHS r Action
-- action = 'action <=> empty
--  <|> Select      <#> "sell"
--  <|> Copy        <#> "cop"
--  <|> Cut         <#> "kill"
--  <|> Delete      <#> "del"
--  <|> Transpose   <#> "trans"
--  <|> Google      <#> "google"

-- data Slice = Whole | Backwards | Forwards  deriving (Show,Eq,Ord,Enum)
-- -- slice :: DNSEarleyRHS r Slice
-- slice = 'slice
--  <=> Whole     <#> "whole"
--  <|> Backwards <#> "back"
--  <|> Forwards  <#> "for"

-- data Region
--  = That
--  | Character
--  | Word_                        -- ^ e.g. @"camelCase"@, @"lisp-case"@, @"snake_case"@ are all two 'Word_'s
--  | Token                        -- ^ e.g. @"camelCase"@, @"lisp-case"@, @"snake_case"@ are all one 'Token's
--  | Group                        -- ^ 'Bracket's delimit 'Group's (e.g. @"(...)"@ or @"<...>"@ or @"[|...|]"@)
--  | Line
--  | Rectangle
--  | Block
--  | Page
--  | Screen
--  | Everything
--  | Definition
--  | Function_
--  | Reference
--  | Structure
--  deriving (Show,Eq,Ord,Enum)
-- -- region :: DNSEarleyRHS r Region
-- region = 'region
--  <=> That       <#> "that"
--  <|> Character  <#> "char"
--  <|> Word_      <#> "word"
--  <|> Token      <#> "toke"
--  <|> Group      <#> "group"
--  <|> Line       <#> "line"
--  <|> Rectangle  <#> "wreck"
--  <|> Block      <#> "block"
--  <|> Page       <#> "page"
--  <|> Screen     <#> "screen"
--  <|> Everything <#> "all"
--  <|> Definition <#> "def"
--  <|> Function_  <#> "fun"
--  <|> Reference  <#> "ref"
--  <|> Structure  <#> "struct"

-- -- data Phrase
-- --  -- continuation necessary
-- --  = Case             Casing                       Phrase
-- --  | Join             Joiner                       Phrase
-- --  | Surround         Brackets                     Phrase
-- --  | Separated        Separator                    Phrase
-- --  | Spelled          [Char]                       Phrase
-- --  | Letter           Char                         Phrase
-- --  | Cap              Char                         Phrase
-- --  | Pasted                                        Phrase
-- --  | Blank                                         Phrase
-- --  -- continuation optional
-- --  | Escaped     Keyword                    (Maybe Phrase)
-- --  | Quoted      Dictation                  (Maybe Phrase)
-- --  | Dictated    Dictation                  (Maybe Phrase)
-- --  -- continuation prohibited
-- --  | Dictation_ Dictation
-- --  deriving (Show,Eq,Ord)

-- -- data Casing = Upper | Lower | Capper deriving (Show,Eq,Ord,Enum)
-- -- data Joiner = Joiner String | CamelJoiner | ClassJoiner deriving (Show,Eq,Ord)
-- -- data Brackets = Brackets String String deriving (Show,Eq,Ord)
-- -- newtype Separator = Separator String  deriving (Show,Eq,Ord)
-- -- newtype Keyword = Keyword String deriving (Show,Eq,Ord)
-- -- newtype Dictation = Dictation [String] deriving (Show,Eq,Ord)

-- -- bracket :: Char -> Brackets
-- -- bracket c = Brackets [c] [c]



-- -- phrase = "phrase"
-- --   -- continuation necessary
-- --   <=> Case       <$>              casing <*>                              phrase
-- --   <|> Join       <$>              joiner <*>                              phrase
-- --   <|> Surround   <$>            brackets <*>                              phrase
-- --   <|> Separated  <$>           separator <*>                              phrase
-- --   <|> Spelled    <$ "spell"              <*> (char-++) <*>                phrase
-- --   <|> Letter     <$>                char <*>                              phrase
-- --   <|> Cap        <$ "cap"                <*>     char <*>                 phrase
-- --   <|> Pasted     <$ "paste"              <*>                              phrase
-- --   <|> Blank      <$ "blank"              <*>                              phrase
-- --   -- continuation optional
-- --   <|> Escaped    <$ "lit"                <*>   keyword <*>               ( phrase-?)
-- --   <|> Quoted     <$ "quote"              <*>   word    <*> ("unquote" *> ( phrase-?))
-- --   <|> Dictated   <$>                word <*>                             ( phrase-?)
-- --   -- continuation prohibited
-- --   <|> Dictation_ <$> word

-- -- separator = "separator"
-- --   <=> Separator ""  <$ "break"
-- --   <|> Separator " " <$ "space"
-- --   <|> Separator "," <$ "comma"
-- --   <|> Separator "/" <$ "slash"
-- --   <|> Separator "." <$ "dot"

-- -- casing = "casing"
-- --   <=> Upper  <$ "upper"
-- --   <|> Lower  <$ "lower"
-- --   <|> Capper <$ "capper"

-- -- joiner = "joiner"
-- --   <=> (\c -> Joiner [c]) <$ "join" <*> char
-- --   <|> Joiner "_"  <$ "snake"
-- --   <|> Joiner "-"  <$ "dash"
-- --   <|> Joiner "/"  <$ "file"
-- --   <|> Joiner ""   <$ "squeeze"
-- --   <|> CamelJoiner <$ "camel"
-- --   <|> ClassJoiner <$ "class"

-- -- brackets = "brackets"
-- --   <=> bracket          <$ "round" <*> char
-- --   <|> Brackets "(" ")" <$ "par"
-- --   <|> Brackets "[" "]" <$ "square"
-- --   <|> Brackets "{" "}" <$ "curl"
-- --   <|> Brackets "<" ">" <$ "angle"
-- --   <|> bracket '"'      <$ "string"
-- --   <|> bracket '\''     <$ "ticked"
-- --   <|> bracket '|'      <$ "norm"

-- char = 'char
--   <=> '`' <#> "grave"

-- -- keyword = "keyword"
-- --   <=> Keyword <$> liftLeaf anyWord "keyword"

-- -- dictation = "dictation"
-- --   <=> Dictation <$> liftLeaf (some anyWord) "dictation"

-- -- word = "word"
-- --   <=> (Dictation . (:[])) <$> liftLeaf anyWord "word"



-- -- phrase_ = "phrase"
-- --  <=> snoc <$> ((phraseA <|> phraseB <|> phraseW)-*) <*> (phraseB <|> phraseC <|> phraseD)

-- phraseB = 'phraseB <=> empty
--  -- TODO letters grammar that consumes tokens with multiple capital letters, as well as tokens with single aliases
--  -- <|> Spelled_  <$> "spell" # letters -- only, not chars
--  <|> Spelled_  <#> "spell" # (char-++)
--  <|> Capped_   <#> "caps" # (char-++)
--  -- <$> alphabetRHS

-- phraseC = 'phraseC <=>
--  Dictated_ <#> "say" # dictation_

-- phraseW = 'phraseW <=>
--  (Dictated_ . P.Dictation . (:[])) <#> word_

-- phraseD = 'phraseD <=>
--  Dictated_ <#> dictation_

-- separator_ = 'separator
--   <=> P.Separator ""  <#> "break"
--   <|> P.Separator " " <#> "space"
--   <|> P.Separator "," <#> "comma"
--   <|> P.Separator "/" <#> "slash"
--   <|> P.Separator "." <#> "dot"

-- casing_ = 'casing
--   <=> P.Upper  <#> "upper"
--   <|> P.Lower  <#> "lower"
--   <|> P.Capper <#> "capper"

-- joiner_ = 'joiner
--   <=> (\c -> P.Joiner [c]) <#> "join" # char
--   <|> P.Joiner "_"  <#> "snake"
--   <|> P.Joiner "-"  <#> "dash"
--   <|> P.Joiner "/"  <#> "file"
--   <|> P.Joiner ""   <#> "squeeze"
--   <|> P.CamelJoiner <#> "camel"
--   <|> P.ClassJoiner <#> "class"

-- brackets_ = 'brackets
--   <=> P.bracket          <#> "round" # char
--   <|> P.Brackets "(" ")" <#> "par"
--   <|> P.Brackets "[" "]" <#> "square"
--   <|> P.Brackets "{" "}" <#> "curl"
--   <|> P.Brackets "<" ">" <#> "angle"
--   <|> P.bracket '"'      <#> "string"
--   <|> P.bracket '\''     <#> "ticked"
--   <|> P.bracket '|'      <#> "norm"

-- dictation_ = dragonGrammar 'dictation_
--  ((P.Dictation . fmap T.unpack) <$> some anyWord)
--  (DGNDictation)
-- {-# NOINLINE dictation_ #-} --TODO doesn't help with the unshared <dictation__4>/<dictation__14>/<dictation__16>

-- word_ = dragonGrammar 'word_
--  (T.unpack <$> anyWord)
--  (DGNWords)

-- keyword_ = dragonGrammar 'keyword_
--  (T.unpack <$> anyWord)
--  (DGNWords)
--  -- <=> Keyword <$> word_


-- -- ================================================================ --

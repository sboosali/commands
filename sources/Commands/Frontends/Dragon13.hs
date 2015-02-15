{-# LANGUAGE DataKinds, GADTs, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RankNTypes, ViewPatterns                             #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- | pretty printer combinators for format readability.
--
module Commands.Frontends.Dragon13 where
import           Commands.Etc
import           Commands.Frontends.Dragon13.Text
import           Commands.Frontends.Dragon13.Types
import           Commands.Instances                ()
import           Control.Monad                     ((<=<))
import           Control.Monad.Catch               (SomeException (..))
import           Data.Bifoldable
import           Data.Bifunctor                    (first, second)
import           Data.Bitraversable
import           Data.Either.Validation            (Validation,
                                                    eitherToValidation,
                                                    validationToEither)
import           Data.Foldable                     (toList)
import           Data.List                         (nub)
import           Data.Monoid                       ((<>))
import qualified Data.Text.Lazy                    as T
import           Language.Python.Version2.Parser   (parseModule)
import           Text.PrettyPrint.Leijen.Text      hiding ((<>))


-- | serialize a grammar into a Python file, unless:
--
-- * the grammars terminals/non-terminals don't lex
-- * the Python file doesn't parse
--
serialize :: DNSGrammar Text Text -> Either [SomeException] Text
serialize = isPythonFile
 <=< (second (displayT . renderPretty 1.0 80 . serializeGrammar) . escapeDNSGrammar)

-- | preserves the input when a valid Python file,
-- reports the syntax error otherwise.
--
-- a "Kleisli arrow" (?).
--
isPythonFile :: Text -> Either [SomeException] Text
isPythonFile s = case parseModule (T.unpack s) "" of
 Right {} -> Right s
 Left  e  -> Left [SomeException e]

-- |
--
serializeGrammar :: DNSGrammar DNSName DNSText -> Doc
serializeGrammar grammar = grammar_
 where
 grammar_ = vsep $ punctuate "\n"
  [ "_commands_rules_ =" <+> "'''"
  , rules_
  , "'''"
  , "_commands_lists_ =" <+> lists_
  ]
 rules_ = serializeRules grammar
 lists_ = serializeLists grammar

-- |
--
-- @
-- <dgndictation> imported;
-- <dgnwords> imported;
-- <dgnletters> imported;
-- <BLANKLINE>
-- <command> exported  = "git" <subcommand>
--                       [({flag})+]
--                     | "ls";
-- <BLANKLINE>
-- <subcommand>  = "status" | <dgndictation>;
-- <BLANKLINE>
-- {flag}  = "force"
--         | "recursive"
--         | "all"
--         | "interactive";
-- @
--
-- any "hung" lines exceeded the width of 50 characters:
--
-- @
-- {flag}  = "force" | "recursive" | "all" | "interactive";
-- ---------10--------20--------30--------40--------50=====
-- @
--
-- this improves readability of long rules.
--
--
--
serializeRules :: DNSGrammar DNSName DNSText -> Doc
serializeRules (DNSGrammar export productions) = serializeImports dnsHeader <> line
 <$$>
 ( cat
 . punctuate "\n"
 . (serializeExport export :)
 . concatMap serializeProduction
 $ productions)

-- | for readability, only insert one newline, between imports. not
-- two, as between productions.
serializeImports :: [DNSProduction False DNSName DNSText] -> Doc
serializeImports = vsep . concatMap serializeProduction

-- | just like @'serializeProduction' ('DNSProduction' ...)@, only
-- with an @"exported"@ inserted.
serializeExport :: DNSProduction True DNSName DNSText -> Doc
serializeExport (DNSProduction l (toList -> rs)) =
 serializeLHS l <+> "exported" <+> encloseSep " = " ";" " | " (map serializeRHS rs)

-- |
--
-- we have:
--
-- @'serializeProduction' 'DNSVocabulary'{} = 'empty'@
--
-- see 'serializeVocabulary' for the 'DNSVocabulary' case.
--
serializeProduction :: DNSProduction False DNSName DNSText -> [Doc]
serializeProduction (DNSProduction l (toList -> rs)) = (:[]) $
 serializeLHS l <+> encloseSep " = " ";" " | " (map serializeRHS rs)
serializeProduction (DNSImport l) = (:[]) $
 serializeLHS l <+> "imported" <> ";"
serializeProduction DNSVocabulary{} = [] -- mempty isn't identity to vertical alignment

--  |  align, fo r readability
-- consolidate DNSAlternatives with concat, as a rewrite, an
-- optimization for readability
serializeRHS :: DNSRHS DNSName DNSText -> Doc
serializeRHS (DNSTerminal t)      = serializeToken t
serializeRHS (DNSNonTerminal l)   = serializeLHS l
serializeRHS (DNSSequence (toList -> rs))     = align . fillSep . map serializeRHS $ rs
serializeRHS (DNSAlternatives (toList -> rs)) = "(" <> (cat . punctuate " | " . map serializeRHS $ rs) <> ")"
serializeRHS (DNSOptional r)      = "[" <> serializeRHS r <> "]"
serializeRHS (DNSMultiple r)      = "(" <> serializeRHS r <> ")+"

serializeLHS :: DNSLHS lhs DNSName -> Doc
serializeLHS (DNSRule (DNSName s)) = "<" <> text s <> ">"
serializeLHS (DNSBuiltin b)        = "<" <> (text . T.toLower . T.pack . show $ b) <> ">"
serializeLHS (DNSList (DNSName s)) = "{" <> text s <> "}"

-- |
--
-- ignores the "written" field of 'DNSPronounced', only serializing
-- the "pronounced" field.
serializeToken :: DNSToken DNSText -> Doc
serializeToken (DNSToken (DNSText s)) = dquotes (text s)
serializeToken (DNSPronounced _ (DNSText s)) = dquotes (text s)

-- | import all 'DNSBuiltins', whether used or not.
dnsHeader :: [DNSProduction False name token]
dnsHeader = map (DNSImport . DNSBuiltin) constructors

serializeLists :: DNSGrammar DNSName DNSText -> Doc
serializeLists = serializeVocabularies . dnsProductions

-- |
-- 'serializeVocabulary' is separate (and very different) from
-- 'serializeProduction'.
-- this is because a 'DNSList' must be mutated in Python,
-- not (as the 'DNSRules' are) defined in the @gramSpec@ string.
serializeVocabularies :: [DNSProduction False DNSName DNSText] -> Doc
serializeVocabularies
 = enclosePythonic "{" "}" ","
 . concatMap serializeVocabulary

serializeVocabulary :: DNSProduction False DNSName DNSText -> [Doc]
serializeVocabulary (DNSVocabulary (DNSList (DNSName n)) (toList -> ts)) = (:[]) $
 (dquotes (text n)) <> ":" <+> enclosePythonic "[" "]" "," (map serializeToken ts)
serializeVocabulary _ = []
-- serializeVocabulary _ = mempty -- not identity to all operations on documents

-- | splits a multi-line collection-literal with the separator at the
-- end of each line, not at the start.
--
-- compare Python-layout, via 'enclosePythonic':
--
-- @
-- {"command": ["git",
--              "rm"],
--  "flag": ["force",
--           "recursive",
--           "all",
--           "interactive"]}
-- @
--
-- against Haskell-layout, via 'encloseSep':
--
-- @
-- {"command": ["git"
--             ,"rm"]
-- ,"flag": ["force"
--          ,"recursive"
--          ,"all"
--          ,"interactive"]}
-- @
--
-- which doesn't parse as Python.
--
enclosePythonic :: Doc -> Doc -> Doc -> [Doc] -> Doc
enclosePythonic left right sep ds
  = left
 <> (align . cat $ punctuate sep ds)
 <> right

-- |
--
--
-- data DNSFormat = DNSFormat
--  { dnsRules :: Text
--  , dnsLists :: Text             --  ^ json?
--  }

-- |
--
-- a 'bifoldMap'.
--
getWords :: (Eq t) => DNSGrammar n t -> [t]
getWords = nub . bifoldMap (const []) (:[])

-- |
--
-- a 'bifoldMap'.
--
getNames :: (Eq n) => DNSGrammar n t -> [n]
getNames = nub . bifoldMap (:[]) (const [])

-- |
--
-- converts the Either to a 'Validation' as an intermediary
-- structure, to report all errors, not just the first.
--
-- a 'bitraverse'.
--
-- a "Kleisli arrow" (?)
escapeDNSGrammar :: DNSGrammar Text Text -> Either [SomeException] (DNSGrammar DNSName DNSText)
escapeDNSGrammar = validationToEither . bitraverse (eitherToValidations . escapeDNSName) (eitherToValidations . escapeDNSText)

-- | 'Validation' is just an Applicative (not Monad) because it doesn't short-circuit, running every computation to monoidally append all errors together
eitherToValidations :: Either e a -> Validation [e] a
eitherToValidations = eitherToValidation . first (:[])


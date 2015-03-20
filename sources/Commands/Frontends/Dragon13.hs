{-# LANGUAGE DataKinds, GADTs, NamedFieldPuns, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms, RankNTypes, ViewPatterns           #-}
-- | Uses pretty printer combinators for readability of serialization.
--
--
module Commands.Frontends.Dragon13 where

import           Commands.Etc
import           Commands.Frontends.Dragon13.Lens
import           Commands.Frontends.Dragon13.Text
import           Commands.Frontends.Dragon13.Types
import           Commands.Instances                ()

import           Control.Lens
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
import           Data.List.NonEmpty                (NonEmpty (..))
import           Data.Maybe                        (mapMaybe)
import           Data.Monoid                       ((<>))
import qualified Data.Text.Lazy                    as T
import           Language.Python.Version2.Parser   (parseModule)
import           Text.PrettyPrint.Leijen.Text      hiding ((<>))


{- $setup

>>> :set -XOverloadedLists -XOverloadedStrings
>>> :{
let root = DNSProduction (DNSRule "root")
            [ DNSSequence
              [                           DNSNonTerminal (SomeDNSLHS (DNSList "command"))
              ,                           DNSNonTerminal (SomeDNSLHS (DNSRule "subcommand"))
              , DNSOptional (DNSMultiple (DNSNonTerminal (SomeDNSLHS (DNSList "flag"))))
              ]
            , DNSTerminal (DNSToken "ls")
            ]
    flag = DNSVocabulary (DNSList "flag")
            [ DNSPronounced "-f" "force"
            , DNSPronounced "-r" "recursive"
            , DNSPronounced "-a" "all"
            , DNSPronounced "-i" "interactive"
            ]
    command = DNSVocabulary (DNSList "command")
                      [ DNSToken "git"
                      , DNSToken "rm"
                      ]
    subcommand = DNSProduction (DNSRule "subcommand")
                       [ DNSTerminal    (DNSToken "status")
                       , DNSNonTerminal (SomeDNSLHS (DNSBuiltin DGNDictation))
                       ]
    Right grammar = escapeDNSGrammar (DNSGrammar root [] [command, subcommand, flag])
:}

(this 'DNSGrammar' is complete/minimal: good for testing, bad at making sense).
-}

-- | serialize a grammar into a Python file, unless:
--
-- * the grammars terminals/non-terminals don't lex (with 'escapeDNSGrammar')
-- * the Python file doesn't parse (with 'isPythonFile')
--
--
--
--
serialize :: DNSGrammar Text Text -> Either [SomeException] Text
serialize = isPythonFile
 <=< (second (display . serializeGrammar) . escapeDNSGrammar)

-- | serializes a grammar into two Python assignments.
--
--
-- >>> serializeGrammar grammar
-- _commands_rules_ = '''
-- <BLANKLINE>
-- <dgndictation> imported;
-- <dgnwords> imported;
-- <dgnletters> imported;
-- <BLANKLINE>
-- <root> exported  = {command}
--                    <subcommand> [({flag})+]
--                  | "ls";
-- <BLANKLINE>
-- <subcommand>  = "status"
--               | <dgndictation>;
-- <BLANKLINE>
-- '''
-- <BLANKLINE>
-- _commands_lists_ = {"command": ["git",
--                                 "rm"],
--                     "flag": ["force",
--                              "recursive",
--                              "all",
--                              "interactive"]}
--
--
-- as you can see, horizontally delimited 'Doc'uments are vertically aligned iff they are too wide. 'encloseSep' and 'enclosePythonic' provide this behavior. this improves readability of long grammars. when things are good, the serialized grammar is loaded by another program (NatLink) anyway. when things go bad, it's good to have a format a human can read, to ease debugging.
--
-- ('DNSImport's don't need to be at the start of the grammar or even above the production that uses it. but it looks nice)
--
serializeGrammar :: DNSGrammar DNSName DNSText -> Doc
serializeGrammar grammar = grammar_
 where
 grammar_ = vsep $ punctuate "\n"
  [ "_commands_rules_ =" <+> "'''"
  , imports_
  , rules_
  , "'''"
  , "_commands_lists_ =" <+> lists_
  ]
 imports_ = serializeImports (dnsHeader <> grammar ^. dnsImports)
 rules_ = serializeRules grammar
 lists_ = serializeLists grammar

-- | serializes a grammar into a Python string.
--
-- imports all 'DNSBuiltins', whether used or not.
--
--
serializeRules :: DNSGrammar DNSName DNSText -> Doc
serializeRules (DNSGrammar{_dnsExport,_dnsProductions})
 = cat
 . punctuate "\n"
 . (serializeExport _dnsExport :)
 . concatMap serializeProduction
 $ _dnsProductions

-- | not unlike 'serializeProduction', but only inserts one newline between imports, not
-- two as between productions, for readability.
--
-- >>> serializeImports dnsHeader
-- <dgndictation> imported;
-- <dgnwords> imported;
-- <dgnletters> imported;
--
serializeImports :: [DNSImport DNSName] -> Doc
serializeImports = vsep . fmap (\l ->serializeLHS l <+> "imported" <> ";")

-- | like @'serializeProduction' ('DNSProduction' ...)@, only
-- it inserts @"exported"@.
--
-- >>> serializeExport $ DNSProduction (DNSRule (DNSName "rule")) [DNSTerminal (DNSToken (DNSText "hello"))]
-- <rule> exported  = "hello";
--
serializeExport :: DNSProduction True DNSName DNSText -> Doc
serializeExport (DNSProduction l (toList -> rs)) =
 serializeLHS l <+> "exported" <+> encloseSep " = " ";" " | " (fmap serializeRHS rs)

-- |
--
-- >>> serializeProduction $ DNSProduction (DNSRule (DNSName "rule")) [DNSTerminal (DNSToken (DNSText "hello"))]
-- <rule>  = "hello";
-- >>> serializeProduction $ DNSVocabulary undefined undefined :: Maybe Doc
-- Nothing
--
-- see 'serializeVocabulary' for the 'DNSVocabulary' case.
--
serializeProduction :: DNSProduction False DNSName DNSText -> Possibly Doc
serializeProduction (DNSProduction l (toList -> rs)) = return $
 serializeLHS l <+> encloseSep " = " ";" " | " (fmap serializeRHS rs)
serializeProduction DNSVocabulary{} = failed "serializeProduction"
-- serializeProduction DNSVocabulary{} = mempty -- not identity to vertical alignment

{- |

>>> :{
serializeRHS $ DNSAlternatives
 [ DNSSequence
   [ DNSTerminal (DNSToken (DNSText "hello"))
   , DNSTerminal (DNSToken (DNSText "world"))
   ]
 , DNSOptional (DNSMultiple (DNSNonTerminal (SomeDNSLHS (DNSRule (DNSName "word")))))
 ]
:}
("hello" "world" | [(<word>)+])

-}
serializeRHS :: DNSRHS DNSName DNSText -> Doc
serializeRHS (DNSTerminal t)      = serializeToken t
serializeRHS (DNSNonTerminal (SomeDNSLHS l))   = serializeLHS l
serializeRHS (DNSOptional r)      = "[" <> serializeRHS r <> "]"
serializeRHS (DNSMultiple r)      = "(" <> serializeRHS r <> ")+"
serializeRHS (DNSSequence     (toList -> rs)) = align . fillSep . fmap serializeRHS $ rs
serializeRHS (DNSAlternatives (toList -> rs)) = "(" <> (cat . punctuate " | " . fmap serializeRHS $ rs) <> ")"

-- |
--
-- >>> serializeLHS $ DNSRule (DNSName "rule")
-- <rule>
-- >>> serializeLHS $ DNSBuiltin DGNDictation
-- <dgndictation>
-- >>> serializeLHS $ DNSList (DNSName "list")
-- {list}
--
--
serializeLHS :: DNSLHS l DNSName -> Doc
serializeLHS (DNSList (DNSName s)) = "{" <> text s <> "}"
serializeLHS (DNSRule (DNSName s)) = "<" <> text s <> ">"
serializeLHS (DNSBuiltin b)        = "<" <> text s <> ">"
 where s = T.pack $ displayDNSBuiltin b

-- | wraps tokens containing whitespace with 'dquotes'.
--
-- ignores the "written" field of 'DNSPronounced', only serializing
-- the "pronounced" field.
--
-- >>> serializeToken (DNSToken (DNSText "text with spaces"))
-- "text with spaces"
-- >>> serializeToken (DNSPronounced undefined (DNSText "pronounced"))
-- "pronounced"
--
serializeToken :: DNSToken DNSText -> Doc
serializeToken (DNSToken (DNSText s))        = dquotes (text s)
serializeToken (DNSPronounced _ (DNSText s)) = dquotes (text s)

dnsHeader :: [DNSImport DNSName]
dnsHeader = fmap DNSBuiltin constructors

-- | serialize the 'DNSList's that were ignored by 'serializeRules'.
--
serializeLists :: DNSGrammar DNSName DNSText -> Doc
serializeLists = serializeVocabularies . _dnsProductions

-- |
--
-- 'serializeVocabulary' is implemented differently from
-- 'serializeProduction', even though conceptually they are
-- both 'DNSProduction's. a 'DNSList' must be mutated in Python,
-- not (as the 'DNSRules' are) defined as a @gramSpec@ String.
--
-- serializeVocabularies $ [DNSVocabulary (DNSList (DNSName "list")) [DNSText "one", DNSText "two", DNSText "three"], DNSVocabulary (DNSList (DNSName "empty")) []]
-- {"list": ["one", "two", "three"], "empty": []}
--
--
serializeVocabularies :: [DNSProduction False DNSName DNSText] -> Doc
serializeVocabularies
 = enclosePythonic "{" "}" ","
 . mapMaybe serializeVocabulary

-- | serializes a 'DNSVocabulary' into a Python dictionary.
--
-- serializeVocabulary $ DNSVocabulary (DNSList (DNSName "list")) [DNSText "one", DNSText "two", DNSText "three"]
-- "list": ["one", "two", "three"]
--
-- a safe partial-function (outputs a list).
serializeVocabulary :: DNSProduction False DNSName DNSText -> Possibly Doc
serializeVocabulary (DNSVocabulary (DNSList (DNSName n)) ts) = return $
 (dquotes (text n)) <> ":" <+> enclosePythonic "[" "]" "," (fmap serializeToken ts)
serializeVocabulary _ = failed "serializeVocabulary"
-- serializeVocabulary _ = mempty -- not identity to all operations on documents

-- | splits a multi-line collection-literal with the separator at the
-- end of each line, not at the start.
--
-- compare Python-layout, via 'enclosePythonic' (which parses as Python):
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

-- | validates the grammar (@:: 'DNSGrammar' name token@) :
--
-- * accepting @name@s with 'escapeDNSName'
-- * accepting @token@s with 'escapeDNSText'
--
-- converts the 'Either' to a 'Validation' and back,
-- to report all errors, not just the first.
--
-- a 'bitraverse'.
--
-- a Kleisli arrow (?) where @m ~ Either [SomeException]@.
--
escapeDNSGrammar :: DNSGrammar Text Text -> Either [SomeException] (DNSGrammar DNSName DNSText)
escapeDNSGrammar = validationToEither . bitraverse (eitherToValidations . escapeDNSName) (eitherToValidations . escapeDNSText)

-- | @Either@ is a @Monad@: it short-circuits. 'Validation' is an @Applicative@, but not a @Monad@: under @traverse@ (or @bitraverse@), it runs the validation (@:: a -> f b@) on every field (@:: a@) in the traversable (@:: t a@), monoidally appending together all errors, not just the first.
eitherToValidations :: Either e a -> Validation [e] a
eitherToValidations = eitherToValidation . first (:[])

-- | preserves the input when a valid Python file (with 'parseModule'),
-- reports the syntax error otherwise.
--
-- a Kleisli arrow (?)
--
-- TODO change to check that with two expressions are a Dictionary and a String
--
isPythonFile :: Text -> Either [SomeException] Text
isPythonFile s = case parseModule (T.unpack s) "" of
 Right {} -> Right s
 Left  e  -> Left [SomeException e]

-- | get all the names in the left-hand sides of the grammar, without duplicates.
--
-- a 'bifoldMap' on the left.
--
-- e.g. @getNames :: (Eq t) => DNSGrammar n t -> [t]@
--
-- >>> map unDNSName $ getNames grammar
-- ["root","command","subcommand","flag"]
--
getNames :: (Eq n, Bifoldable p) => p n t -> [n]
getNames = nub . bifoldMap (:[]) (const [])

-- | get all the words in the terminals of the grammar, without duplicates.
--
-- a 'bifoldMap' on the right.
--
-- e.g. @getWords :: (Eq t) => DNSGrammar n t -> [t]@
--
-- >>> map unDNSText $ getWords grammar
-- ["ls","git","rm","status","-f","force","-r","recursive","-a","all","-i","interactive"]
--
getWords :: (Eq t, Bifoldable p) => p n t -> [t]
getWords = nub . bifoldMap (const []) (:[])

hoistDNSRHS :: (DNSRHS n t -> DNSRHS n t) -> DNSProduction True n t -> NonEmpty (DNSRHS n t)
hoistDNSRHS f (DNSProduction l _) = f (DNSNonTerminal (SomeDNSLHS l)) :| []


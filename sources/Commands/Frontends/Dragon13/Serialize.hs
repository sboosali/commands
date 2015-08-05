{-# LANGUAGE DataKinds, GADTs, NamedFieldPuns, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms, RankNTypes, ViewPatterns, RecordWildCards          #-}
-- | Uses pretty printer combinators for readability of serialization.
--
--
module Commands.Frontends.Dragon13.Serialize where

import           Commands.Etc
import           Commands.Frontends.Dragon13.Types
import           Commands.Frontends.Dragon13.Text
import           Commands.Frontends.Dragon13.Shim
import           Commands.Frontends.Dragon13.Lens

import           Control.Monad.Catch               (SomeException (..))
import           Data.Bitraversable
import           Data.Either.Validation            (validationToEither)
import           Data.Foldable                     (toList)
import           Data.Monoid                       ((<>))
import qualified Data.Text.Lazy                    as T
import           Text.PrettyPrint.Leijen.Text      hiding ((<>))
import Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe                        (mapMaybe)
import Control.Lens


-- | 
data SerializedGrammar = SerializedGrammar
 { serializedRules  :: Doc
 , serializedLists  :: Doc
 , serializedExport :: Doc
 }

{- $setup

>>> :set -XOverloadedLists -XOverloadedStrings -XNamedFieldPuns
>>> :{
let root = DNSProduction () (DNSRule "root") $ DNSAlternatives
            [ DNSSequence
              [                           DNSNonTerminal (SomeDNSLHS (DNSList "command"))
              ,                           DNSNonTerminal (SomeDNSLHS (DNSRule "subcommand"))
              , DNSOptional (DNSMultiple (DNSNonTerminal (SomeDNSLHS (DNSList "flag"))))
              ]
            , DNSTerminal (DNSToken "ls")
            ]
    subcommand = DNSProduction () (DNSRule "subcommand") $ DNSAlternatives
                       [ DNSTerminal    (DNSToken "status")
                       , DNSNonTerminal (SomeDNSLHS (DNSBuiltinRule DGNDictation))
                       ]
    flag = DNSVocabulary () (DNSList "flag")
            [ DNSPronounced "-f" "force"
            , DNSPronounced "-r" "recursive"
            , DNSPronounced "-a" "all"
            , DNSPronounced "-i" "interactive"
            ]
    command = DNSVocabulary () (DNSList "command")
                      [ DNSToken "git"
                      , DNSToken "rm"
                      ]
    Right grammar = escapeDNSGrammar (DNSGrammar [root, subcommand] [command, flag] dnsHeader)
:}

(this 'DNSGrammar' is complete/minimal: good for testing, bad at making sense).
-}



-- ================================================================ --

{- | serializes an (escaped) grammar into a Python Docstring and a
Python Dict.


>>> let SerializedGrammar{serializedRules,serializedLists,serializedExport} = serializeGrammar grammar
>>> serializedExport
"root"
>>> serializedRules
'''
<BLANKLINE>
<dgndictation> imported;
<dgnwords> imported;
<dgnletters> imported;
<BLANKLINE>
<root> exported  = {command}
                   <subcommand> [({flag})+]
                 | "ls";
<BLANKLINE>
<subcommand>  = "status"
              | <dgndictation>;
<BLANKLINE>
'''

>>> serializedLists
{"command": ["git","rm"],
 "flag": ["force",
          "recursive",
          "all",
          "interactive"]}


as you can see, horizontally delimited 'Doc'uments are vertically aligned iff they are too wide. 'encloseSep' and 'enclosePythonic' provide this behavior. this improves readability of long grammars. when things are good, the serialized grammar is loaded by another program (NatLink) anyway. when things go bad, it's good to have a format a human can read, to ease debugging.

('DNSImport's don't need to be at the start of the grammar or even above the production that uses it. but it looks nice)


-}
serializeGrammar :: DNSGrammar i DNSText DNSName -> SerializedGrammar
serializeGrammar grammar = SerializedGrammar{..}
 where
 serializedLists  = serializeVocabularies (grammar^.dnsVocabularies)
 serializedExport = serializeName name
 serializedRules  = vsep . punctuate "\n" $
  [ "'''"
  , serializeImports     (grammar^.dnsImports)
  , serializeProductions (grammar^.dnsProductions)
  , "'''"
  ]
 serializeName = dquotes . text
 name = unDNSName (grammar^?!dnsExport.dnsProductionLHS.dnsLHSName) -- TODO unsafe

-- | not unlike 'serializeProduction', but only inserts one newline between imports, not
-- two as between productions, for readability.
--
-- >>> serializeImports dnsHeader
-- <dgndictation> imported;
-- <dgnwords> imported;
-- <dgnletters> imported;
--
serializeImports :: [DNSImport DNSName] -> Doc
serializeImports = vsep . fmap (\(DNSImport l) -> serializeLHS l <+> "imported" <> ";")

{- | serializes a 'DNSVocabulary' into a Python @Dict@.

'serializeVocabulary' is implemented differently from
'serializeProduction', even though conceptually they are
both 'DNSProduction's. a 'DNSList' must be mutated in Python,
not (as the 'DNSRule's are) defined as a @gramSpec@ String.

>>> serializeVocabularies []
{}

>>> serializeVocabularies $ [DNSVocabulary () (DNSList (DNSName "list")) [DNSToken (DNSText "one"), DNSToken (DNSText "two")], DNSVocabulary () (DNSList (DNSName "empty")) []]
{"list": ["one","two"],
 "empty": []}

-}
serializeVocabularies
 :: [DNSVocabulary i DNSText DNSName] -> Doc
serializeVocabularies
 = enclosePythonic "{" "}" ","
 . mapMaybe serializeVocabulary

{- | 

>>> serializeVocabulary $ DNSVocabulary () (DNSList (DNSName "list")) [DNSToken (DNSText "one"), DNSToken (DNSText "two")]
"list": ["one","two"]

-}
serializeVocabulary :: DNSVocabulary i DNSText DNSName -> Possibly Doc
serializeVocabulary (DNSVocabulary _ (DNSList (DNSName n)) ts) = return $
 (dquotes (text n)) <> ":" <+> enclosePythonic "[" "]" "," (fmap serializeToken ts)

-- | serializes 'DNSProduction's into a Python @String@.
--
--
--
--
serializeProductions :: NonEmpty (DNSProduction i DNSText DNSName) -> Doc
serializeProductions (export :| productions)
 = cat
 . punctuate "\n"
 $ serializeExport export : fmap serializeProduction productions

-- | like @'serializeProduction' ('DNSProduction' ...)@, only
-- it inserts @"exported"@.
--
-- >>> serializeExport $ DNSProduction undefined (DNSRule (DNSName "rule")) (DNSTerminal (DNSToken (DNSText "token")))
-- <rule> exported  = "token";
--
serializeExport :: DNSProduction i DNSText DNSName -> Doc
serializeExport (DNSProduction _ l (asDNSAlternatives -> toList -> rs))
 = serializeLHS l <+> "exported" <+> encloseSep " = " ";" " | " (fmap serializeRHS rs)

-- |
--
-- >>> serializeProduction $ DNSProduction undefined (DNSRule (DNSName "rule")) (DNSTerminal (DNSToken (DNSText "token")))
-- <rule>  = "token";
--
serializeProduction :: DNSProduction i DNSText DNSName -> Doc
serializeProduction (DNSProduction _ l (asDNSAlternatives -> toList -> rs))
 = serializeLHS l <+> encloseSep " = " ";" " | " (fmap serializeRHS rs)

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
serializeRHS :: DNSRHS DNSText DNSName -> Doc
serializeRHS (DNSTerminal t)                  = serializeToken t
serializeRHS (DNSNonTerminal (SomeDNSLHS l))  = serializeLHS l
serializeRHS (DNSOptional r)                  = "[" <> serializeRHS r <> "]"
serializeRHS (DNSMultiple r)                  = "(" <> serializeRHS r <> ")+"
serializeRHS (DNSSequence     (toList -> rs)) = align . fillSep . fmap serializeRHS $ rs
serializeRHS (DNSAlternatives (toList -> rs)) = "(" <> (cat . punctuate " | " . fmap serializeRHS $ rs) <> ")"

-- |
--
-- >>> serializeLHS $ DNSRule (DNSName "rule")
-- <rule>
-- >>> serializeLHS $ DNSBuiltinRule DGNDictation
-- <dgndictation>
-- >>> serializeLHS $ DNSList (DNSName "list")
-- {list}
--
--
serializeLHS :: DNSLHS l s DNSName -> Doc
serializeLHS (DNSRule (DNSName s)) = "<" <> text s <> ">"
serializeLHS (DNSBuiltinRule b)    = "<" <> text s <> ">"
 where s = T.pack $ displayDNSBuiltinRule b
serializeLHS (DNSList (DNSName s)) = "{" <> text s <> "}"
serializeLHS (DNSBuiltinList b)    = "{" <> text s <> "}"
 where s = T.pack $ displayDNSBuiltinList b

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

{- | splits a multi-line collection-literal with the separator at the
end of each line, not at the start.

compare Python-layout, via 'enclosePythonic' (which parses as Python):

@
{"command": ["git",
             "rm"],
 "flag": ["force",
          "recursive",
          "all",
          "interactive"]}
@

against Haskell-layout, via 'encloseSep':

@
{"command": ["git"
            ,"rm"]
,"flag": ["force"
         ,"recursive"
         ,"all"
         ,"interactive"]}
@

which doesn't parse as Python.

-}
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
-- a Kleisli arrow where @(m ~ Either [SomeException])@
--
escapeDNSGrammar :: DNSGrammar i Text Text -> Either [SomeException] (DNSGrammar i DNSText DNSName)
escapeDNSGrammar = validationToEither . bitraverse (eitherToValidations . escapeDNSText) (eitherToValidations . escapeDNSName)



-- ================================================================ --

data Address = Address Host Port
 deriving (Show,Eq,Ord)
type Host = String
type Port = String

{- | serialize a grammar (with 'serializeGrammar') into a Python file, unless:

* the grammars terminals/non-terminals don't "lex" (with 'escapeDNSGrammar')
* the Python file doesn't parse (with 'newPythonFile')

>>> let Right{} = shimmySerialization "'localhost'" (serializeGrammar grammar)

a Kleisli arrow (when partially applied).

-}
shimmySerialization :: Address -> SerializedGrammar -> Possibly PythonFile
shimmySerialization address = newPythonFile . displayDoc . getShim . from_SerializedGrammar_to_ShimR address

-- | @SerializedGrammar = ShimR - Address@
from_SerializedGrammar_to_ShimR :: Address -> SerializedGrammar -> ShimR Doc
from_SerializedGrammar_to_ShimR (Address (T.pack -> text -> serverHost) (T.pack -> text -> serverPort)) SerializedGrammar{..}
 = ShimR serializedRules serializedLists serializedExport serverHost serverPort

-- | (for debugging)
displaySerializedGrammar :: SerializedGrammar -> Text
displaySerializedGrammar SerializedGrammar{..} =
 displayDoc $ (vsep . punctuate "\n") [serializedExport,serializedRules,serializedLists]


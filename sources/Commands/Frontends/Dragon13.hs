{-# LANGUAGE OverloadedStrings, PatternSynonyms, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- | pretty printer combinators for format readability.
--
module Commands.Frontends.Dragon13 where
import           Commands.Etc
import           Commands.Frontends.Dragon13.Text
import           Commands.Frontends.Dragon13.Types
import           Data.Monoid                       ((<>))
import qualified Data.Text.Lazy                    as T
import           Text.PrettyPrint.Leijen.Text      hiding ((<>))


serialize :: DragonGrammar DragonText -> Text
serialize = serialize' 80

-- | unlimited width, as it will be embedded in a Python file. or limited 80 character went for easier reading, as the format is whitespace insensitive, I think.
serialize' :: Int -> DragonGrammar DragonText -> Text
serialize' width = displayT . renderPretty 1.0 width . serializeGrammar

-- |
--
-- >>> :set -XOverloadedStrings
-- >>> let command = DragonExport (DragonRule "command") (DragonAlternatives [DragonTerminal (DragonToken "git"), DragonNonTerminal (DragonLHSRule (DragonRule "subcommand")), DragonOptional (DragonMultiple (DragonNonTerminal (DragonLHSList (DragonList "flag"))))])
-- >>> let subcommand = DragonProduction (DragonRule "subcommand") (DragonAlternatives [DragonTerminal (DragonToken "status"), DragonNonTerminal (DragonLHSRule (DragonBuiltin DGNDictation))])
-- >>> let flag = DragonVocabulary (DragonList "flag") [DragonPronounced "f" "force", DragonPronounced "r" "recursive"]
-- >>> let grammar = DragonGrammar command [subcommand, flag] :: DragonGrammar Text
-- >>> escaped <- escapeDragonGrammar grammar
-- >>> putStrLn . T.unpack . serialize' 50 $ escaped
-- <dgndictation> imported;
-- <BLANKLINE>
-- <dgnwords> imported;
-- <BLANKLINE>
-- <dgnletters> imported;
-- <BLANKLINE>
-- <command> exported  = "git"
--                     | <subcommand>
--                     | [({flag})+];
-- <BLANKLINE>
-- <subcommand>  = "status" | <dgndictation>;
-- <BLANKLINE>
-- {flag}  = "f"\"force" | "r"\"recursive";
--
-- the "hung" line exceeded the width of 50 characters:
--
-- @
-- \<command> exported  = "git" | \<subcommand> | [({flag})+];
-- ---------10--------20--------30----------40------50======
-- @
--
-- this improves readability of long rules.
--
--
--
serializeGrammar :: DragonGrammar DragonText -> Doc
serializeGrammar (DragonGrammar export productions) = cat . punctuate "\n" . (map serializeProduction header <>) . (serializeExport export :) . map serializeProduction $ productions

header :: [DragonProduction s]
header = map (DragonImport . DragonLHSRule . DragonBuiltin) constructors

-- |
serializeExport :: DragonExport DragonText -> Doc
serializeExport (DragonExport rule (DragonAlternatives rs)) =
 serializeRule rule <+> "exported" <+> encloseSep " = " ";" " | " (map serializeRHS rs)
serializeExport (DragonExport rule r) =
  serializeRule rule <+> "exported" <+> " =" <+> serializeRHS r <> ";"

-- |
--
serializeProduction :: DragonProduction DragonText -> Doc
serializeProduction (DragonProduction rule (DragonAlternatives rs)) =
  serializeRule rule <+> encloseSep " = " ";" " | " (map serializeRHS rs)
serializeProduction (DragonProduction rule r) =
  serializeRule rule <+> " =" <+> serializeRHS r <> ";"
serializeProduction (DragonVocabulary list ts) =
  serializeList list <+> encloseSep " = " ";" " | " (map serializeToken ts)
serializeProduction (DragonImport l) =
  serializeLHS l <+> "imported" <> ";"

-- serializeEquation :: Bool -> DragonLHS DragonText -> Either [DragonToken DragonText] (DragonRHS DragonText) -> Doc

--  |  align, for readability
-- consolidate DragonAlternatives with concat, as a rewrite, an
-- optimization for readability
serializeRHS :: DragonRHS DragonText -> Doc
serializeRHS (DragonTerminal t) = serializeToken t
serializeRHS (DragonNonTerminal l) = serializeLHS l
serializeRHS (DragonOptional r) = "[" <> serializeRHS r <> "]"
serializeRHS (DragonMultiple r) = "(" <> serializeRHS r <> ")+"
serializeRHS (DragonAlternatives rs) = "(" <> (cat . punctuate " | " . map serializeRHS $ rs) <> ")"

serializeLHS :: DragonLHS DragonText -> Doc
serializeLHS (DragonLHSList list) = serializeList list
serializeLHS (DragonLHSRule rule) = serializeRule rule

serializeRule :: DragonLHSRule DragonText -> Doc
serializeRule (DragonRule (DragonText s)) = "<" <> text s <> ">"
serializeRule (DragonBuiltin b) = "<" <> (text . T.toLower . T.pack . show $ b) <> ">"

serializeList :: DragonLHSList DragonText -> Doc
serializeList (DragonList (DragonText s)) = "{" <> text s <> "}"

serializeToken :: DragonToken DragonText -> Doc
serializeToken (DragonToken (DragonText s)) = dquotes (text s)
serializeToken (DragonPronounced (DragonText s1) (DragonText s2)) = dquotes (text s1) <> "\\" <> dquotes (text s2)


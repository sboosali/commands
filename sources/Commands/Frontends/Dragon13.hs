{-# LANGUAGE DataKinds, GADTs, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RankNTypes                                           #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- | pretty printer combinators for format readability.
--
module Commands.Frontends.Dragon13 where
import           Commands.Etc
import           Commands.Frontends.Dragon13.Text
import           Commands.Frontends.Dragon13.Types
import           Data.Bifoldable
import           Data.Bitraversable
import           Data.Monoid                       ((<>))
import qualified Data.Text.Lazy                    as T
import           Text.PrettyPrint.Leijen.Text      hiding ((<>))


-- |
--
serialize :: DNSGrammar DNSName DNSText -> Text
serialize = serialize' 80

-- | unlimited width, as it will be embedded in a Python file. or limited 80 character went for easier reading, as the format is whitespace insensitive, I think.
serialize' :: Int -> DNSGrammar DNSName DNSText -> Text
serialize' width = displayT . renderPretty 1.0 width . serializeGrammar

-- |
--
-- >>> :set -XOverloadedStrings
-- >>> import Commands.Plugins.Example (grammar, command, subcommand, flag)
-- >>> escaped <- escapeDNSGrammar grammar
-- >>> putStrLn . T.unpack . serialize' 50 $ escaped
-- <dgndictation> imported;
-- <BLANKLINE>
-- <dgnwords> imported;
-- <BLANKLINE>
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
--
-- the "hung" lines exceeded the width of 30 characters:
--
-- @
-- {flag}  = "force" | "recursive" | "all";
-- ---------10--------20--------30==========40
-- @
--
-- this improves readability of long rules.
--
--
--
serializeGrammar :: DNSGrammar DNSName DNSText -> Doc
serializeGrammar (DNSGrammar export productions)
 = cat
 . punctuate "\n"
 . (map serializeProduction dnsHeader <>)
 . (serializeExport export :)
 . map serializeProduction
 $ productions

-- |
serializeExport :: DNSProduction True DNSName DNSText -> Doc
serializeExport (DNSProduction l rs) =
 serializeLHS l <+> "exported" <+> encloseSep " = " ";" " | " (map serializeRHS rs)
serializeExport (DNSVocabulary l ts) =
 serializeLHS l <+> "exported" <+> encloseSep " = " ";" " | " (map serializeToken ts)

-- |
--
serializeProduction :: DNSProduction False DNSName DNSText -> Doc
serializeProduction (DNSProduction l rs) =
 serializeLHS l <+> encloseSep " = " ";" " | " (map serializeRHS rs)
serializeProduction (DNSVocabulary l ts) =
 serializeLHS l <+> encloseSep " = " ";" " | " (map serializeToken ts)
serializeProduction (DNSImport l) = serializeLHS l <+> "imported" <> ";"

--  |  align, fo r readability
-- consolidate DNSAlternatives with concat, as a rewrite, an
-- optimization for readability
serializeRHS :: DNSRHS DNSName DNSText -> Doc
serializeRHS (DNSTerminal t)      = serializeToken t
serializeRHS (DNSNonTerminal l)   = serializeLHS l
serializeRHS (DNSSequence rs)     = align . fillSep . map serializeRHS $ rs
serializeRHS (DNSAlternatives rs) = "(" <> (cat . punctuate " | " . map serializeRHS $ rs) <> ")"
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

-- |
--
-- just a 'bitraverse'.
--
escapeDNSGrammar :: DNSGrammar Text Text -> Possibly (DNSGrammar DNSName DNSText)
escapeDNSGrammar grammar = biforM grammar escapeDNSName escapeDNSText

-- |
--
-- just a ''.
--
getTokens :: DNSGrammar n t -> [t]
getTokens = undefined

-- |
--
-- just a ''.
--
getNames :: DNSGrammar n t -> [n]
getNames = undefined

-- |
--
-- just a 'bifoldMap'.
--
getTokensAndNames :: DNSGrammar n t -> ([t],[n])
getTokensAndNames = undefined

{-# LANGUAGE LambdaCase, NamedFieldPuns, OverloadedLists, OverloadedStrings #-}
-- | (See source), or "Commands.Frontends.Dragon13.Serialize"
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-incomplete-uni-patterns #-}
module Commands.Frontends.Dragon13.Example where
import Commands.Frontends.Dragon13

main = do

  let
    root = DNSProduction () (DNSRule "root") $ DNSAlternatives
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
    Right grammar = escapeDNSGrammar (DNSGrammar [root, subcommand] [command, flag] dnsHeader) -- NOTE partial

  let SerializedGrammar{serializedRules,serializedLists,serializedExport} = serializeGrammar grammar

  print root

  print serializedRules

  print serializedLists

  print serializedExport

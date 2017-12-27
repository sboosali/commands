{-# LANGUAGE PostfixOperators, TemplateHaskell, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Phrase
 ( module Commands.Plugins.Spiros.Phrase
 , module Commands.Plugins.Spiros.Phrase.Types
 , module Commands.Plugins.Spiros.Phrase.Run
 , module Commands.Plugins.Spiros.Phrase.Munging
 , module Commands.Plugins.Spiros.Phrase.Spacing
 , module Commands.Plugins.Spiros.Number 
 ) where
import           Commands.Plugins.Spiros.Types
import           Commands.Plugins.Spiros.Phrase.Types
import           Commands.Plugins.Spiros.Number 
import           Commands.Plugins.Spiros.Phrase.Run
import           Commands.Plugins.Spiros.Phrase.Munging
import           Commands.Plugins.Spiros.Phrase.Spacing

import           Commands.Extra
import           Commands.Frontends.Dragon13
import           Commands.Mixins.DNS13OSX9
import qualified Commands.Backends.Workflow as W
import           Commands.Parsers.Earley as CP

import qualified Data.Text.Lazy                   as T

import           Data.Char


phraseCommand :: DNSEarleyCommand SpirosContext SpirosMonad_ Phrase
phraseCommand = Command phrase bestPhrase $ \_ p -> do
 s <- W.getClipboard
 W.sendText (runPhrase_ defSpacing s p)

-- ================================================================ --

-- -- | transforms "token"s from 'phrase' into an "s-expression" with 'pPhrase'.
-- phrase = pPhrase <$> phrase

phrase :: DNSEarleyRHS Phrase
phrase = Phrase <$> complexGrammar 'phrase
 -- NOTE phrase is still a NonTerminal, given instance Applicative RHS, because:
 -- either: (f <$> NonTerminal ...) is (fmap f (NonTerminal ...)) is (NonTerminal ... ) 
 -- or:     (f <$> NonTerminal ...) is (Pure f <*> NonTerminal ...) is (NonTerminal ... ) 

 -- (conssnoc <$> (phraseA) <*> ((phraseA <|> phraseB <|> phraseW)-*) <*> (phraseB <|> phraseC <|> phraseD))
 -- (conssnoc <$> (phraseA) <*> ((phraseA <|> phraseB <|> phraseD)-*) <*> (phraseB <|> phraseC <|> phraseD))
 -- where
 -- conssnoc x ys z = [x] <> ys <> [z]

 (snoc     <$>             ((phraseA <|> phraseB <|> phraseW)-*) <*> (phraseB <|> phraseC <|> phraseD)) -- the parser 
 (snoc     <$>             ((phraseA <|> phraseB <|> phraseD)-*) <*> (phraseB <|> phraseC <|> phraseD)) -- the grammar 

-- | a sub-phrase where a phrase to the right is certain.
--
-- this ordering prioritizes the escaping Escaped_/Quoted_ over the
-- escaped, e.g. "quote greater equal unquote".
phraseA :: DNSEarleyRHS Phrase_
phraseA = 'phraseA <=> empty
 <|> pasted 
 <|> Blank_      <$ "blank"
 -- <|> Spelled_    <$ "spell" # (character-++)
 -- <|> Spelled_    <$ "lets" # letters -- (letter-++)
 <|> Bonked_     <$  "smack"   -- (like saying "break" enough times) 
 <|> Separated_  <$> separator
 <|> Cased_      <$> casing
 <|> Joined_     <$> joiner
 <|> Surrounded_ <$> brackets
 <|> Splitted_   <$> splitter 

-- | a sub-phrase where a phrase to the right is possible.
phraseB :: DNSEarleyRHS Phrase_
phraseB = 'phraseB <=> empty
 <|> Escaped_  <$ "litter"            <*> keyword         -- abbreviation for "literally" 
 <|> Quoted_   <$ "quote"             <*> dictation <* "unquote"

 -- <|> Spelled_  <$ ("let's" <|> "let") <*> (character-++)  -- conflicts with "let" in Haskell 
 <|> Spelled_  <$ "let's" <*> (character-++)  -- abbreviation for "letters" 
 <|> Capped_   <$ "caps"              <*> (character-++)  -- abbreviation for "capital letters" 
 <|> Capped_   <$ "shrimp"            <*> (character-++)  -- abbreviation for "symbol", that's frequent

 <|> pasted 
 <|> Blank_     <$ "blank"

 <|> Spelled_  <$> (phoneticAlphabetRHS-++)  -- last, since it's unprefixed 

 -- TODO letters grammar that consumes tokens with multiple capital letters, as well as tokens with single aliases
 -- <|> Spelled_  <$ "spell" <*> letters -- only, not characters

-- | a sub-phrase where a phrase to the right is impossible.
phraseC :: DNSEarleyRHS Phrase_
phraseC = 'phraseC <=> Dictated_ <$ "say" <*> dictation

-- | injects word_ into phrase
phraseW :: DNSEarleyRHS Phrase_
phraseW = 'phraseW <=> word2phrase_ <$> word_

-- | injects dictation into phrase_
phraseD :: DNSEarleyRHS Phrase_
phraseD = 'phraseD <=> Dictated_ <$> dictation

pasted :: DNSEarleyRHS Phrase_ 
pasted = 'pasted 
 <=> Pasted_     <$ "pasted"    -- "yank" 
 <|> Clipboard_  <$ "clip"    -- 

separator = 'separator <=> empty
 <|> Separator ""  <$ "break" --TODO separation should depend on context i.e. blank between symbols, a space between words, space after a comma but not before it. i.e. the choice is delayed until munging.
 <|> Separator " " <$ "space"
 <|> Separator "," <$ "comma"

casing = 'casing
 <=> LowerCase <$ "lower"
 <|> UpperCase <$ "upper"
 <|> CapCase   <$ "copper"              -- "capper" 

joiner = 'joiner
 <=> (\c -> Joiner [c]) <$ "join" <*> character
 <|> Joiner "_" <$ "snake"
 <|> Joiner "-" <$ "dash"
 -- <|> Joiner "/" <$ "file"
 <|> Joiner ""  <$ "squeeze"
 <|> CamelJoiner <$ "camel"    -- "cam"
 <|> ClassJoiner <$ "class"
 <|> ShrinkJoiner <$ "shrink"  -- "shrink plug-in" -> "plugin"

brackets = 'brackets
 <=> bracket          <$ "round" <*> character
 <|> Brackets "(" ")" <$ "par"
 <|> Brackets "[" "]" <$ "square"
 <|> Brackets "{" "}" <$ "curl"
 <|> Brackets "<" ">" <$ "angle"
 <|> bracket '"'      <$ "string"
 <|> bracket '\''     <$ "ticked"
 <|> bracket '|'      <$ "norm"
 -- <|> Brackets "**" "**" <$ "bold"

splitter = 'splitter 
 <=> Splitter <$ "split" 
-- e.g. "split reach_YouTube" -> "reach you tube"  

-- disjoint vocabulary ("effects"), possibly overlapping parses ("results")
character :: DNSEarleyRHS Char
character = 'character <=> empty
 <|> punctuationRHS
 <|> englishNumericRHS
 <|> literalNumericRHS
 <|> phoneticAlphabetRHS

punctuationRHS :: DNSEarleyRHS Char
punctuationRHS = vocab
 [ "grave"-: '`'
 , "till"-: '~'
 , "bang"-: '!'
 , "axe"-: '@'
 , "pound"-: '#'
 , "doll"-: '$'
 , "purse"-: '%'
 , "care"-: '^'
 , "amp"-: '&'
 , "star"-: '*'
 , "lore"-: '('
 , "roar"-: ')'
 , "hit"-: '-'                  -- during a Phrase,Dragon recognizes "dash" literally as "-" 
 , "score"-: '_'
 , "equal"-: '='
 , "plus"-: '+'
 , "lack"-: '['
 , "lace"-: '{'
 , "rack"-: ']'
 , "race"-: '}'
 , "stroke"-: '\\'
 , "pipe"-: '|'
 , "semi"-: ';'
 , "coal"-: ':'
 , "tick"-: '\''
 , "quote"-: '"'
 , "com"-: ','
 , "less"-: '<'
 , "dot"-: '.'
 , "great"-: '>'
 , "slash"-: '/'
 , "quest"-: '?'
 , "tab"-: '\t'
 , "ace"-: ' '
 , "ret"-: '\n'  -- "line" conflicts with (Line :: Region)  
 ]

{- | equivalent to:

@
 <|> '0' <$ "0"
 <|> '1' <$ "1"
 ...
 <|> '9' <$ "9"
@

-}
literalNumericRHS :: DNSEarleyRHS Char
literalNumericRHS = foldMap (\c -> c <$ token [c]) ['0'..'9']

phoneticAlphabetRHS :: DNSEarleyRHS Char
phoneticAlphabetRHS = vocab phoneticAlphabet

phoneticAlphabet :: [(String, Char)] 
phoneticAlphabet =
 [ "alpha"-: 'a'
 , "bravo"-: 'b'
 , "charlie"-: 'c'
 , "delta"-: 'd'
 , "echo"-: 'e'
 , "foxtrot"-: 'f'
 , "golf"-: 'g'
 , "hotel"-: 'h'
 , "india"-: 'i'
 , "juliet"-: 'j'
 , "kilo"-: 'k'
 , "lima"-: 'l'
 , "mike"-: 'm'
 , "november"-: 'n'
 , "oscar"-: 'o'
 , "poppa"-: 'p'
 , "quebec"-: 'q'
 , "romeo"-: 'r'
 , "sierra"-: 's'
 , "tango"-: 't'
 , "uniform"-: 'u'
 , "victor"-: 'v'
 , "whiskey"-: 'w'
 , "x-ray"-: 'x'
 , "yankee"-: 'y'
 , "zulu"-: 'z'
 ]

{- | equivalent to:

@
 <|> 'a' <$ "a"
 <|> 'b' <$ "b"
 <|> 'c' <$ "c"
 <|> ...
 <|> 'z' <$ "z"

 <|> 'a' <$ "A"
 <|> 'b' <$ "B"
 <|> 'c' <$ "C"
 <|> ...
 <|> 'z' <$ "Z"
@

-}
literalAlphabetRHS :: DNSEarleyRHS Char
literalAlphabetRHS = foldMap (\c -> (c <$ token [c]) <|> (c <$ token [toUpper c])) ['a'..'z'] -- TODO What will we get back from Dragon anyway?

dictation :: R Dictation 
dictation = dragonGrammar 'dictation
 ((Dictation . fmap T.unpack) <$> UnsafeEarleyProduction (some CP.anyWord))
 (DGNDictation)
{-# NOINLINE dictation #-} --TODO doesn't help with the unshared <dictation__4>/<dictation__14>/<dictation__16>

word_ :: R String
word_ = dragonGrammar 'word_
 (T.unpack <$> UnsafeEarleyProduction CP.anyWord)
 (DGNWords)

letters :: R Letters              -- TODO rename to dgnletters, but then must be qualified when serialized to avoid conflict 
letters = simpleGrammar 'letters
 ((Letters . T.unpack) <$> UnsafeEarleyProduction CP.anyLetters)
 ((SomeDNSNonTerminal . DNSBuiltinRule) DGNLetters)

keyword :: R Keyword 
keyword = 'keyword
 <=> (Keyword . T.unpack) <$> terminals


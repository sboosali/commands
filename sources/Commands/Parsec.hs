{-# LANGUAGE FlexibleContexts, RankNTypes #-}
-- |
-- wraps "Text.Parsec"
module Commands.Parsec
 ( module Text.Parsec
 -- |
 , module Commands.Parsec
) where
import           Commands.Etc

import           Control.Applicative
import           Control.Monad.Catch (throwM)
import           Text.Parsec         hiding (Parsec, many, parse, space, (<|>))
import qualified Text.Parsec         as Parsec


-- | a parser of strings, that keeps no state, and runs no effects.
--
-- our parsers are context-sensitive, but the context is passed from
-- parent to child as argument. i.e. we would want @Reader@ with
-- @local@ if anything (I think), not "non-local" @State@. hence the
-- unit @State@ @()@.
type Parsec = Parsec.Parsec String ()

-- type Parsec = ParsecT String () Identity
-- commented out, because doctest loads module with different Transformers versions

type Token = String

-- | specialized helper function for easy parsing.
--
-- wraps 'Parsec.parse'
parse :: Parsec a -> String -> Possibly a
parse p s = either throwM return $ Parsec.parse p s s

-- |
--
-- >>> parse (anyWord `manyUntil` (Some . string $ "end")) "one two three end"
-- ["one","two","three"]
--
-- >>> parse (anyWord `manyUntil` (Some . string $ "end")) "one two three"
-- *** Exception: "one two three" (line 1, column 14):
-- unexpected end of input
-- expecting a space, "end" or a word
--
-- >>> parse (anyWord `manyUntil` (Some eof)) "one two three"
-- ["one","two","three"]
--
--
manyUntil :: Parsec a -> Some Parsec -> Parsec [a]
manyUntil p (Some q) = (:) <$> p <*> p `manyTill` (try . lookAhead) q

-- |
--
-- >>> parse anyWord "one-word"
-- "one-word"
--
-- >>> parse anyWord "123"
-- "123"
--
-- >>> parse anyWord "two words"
-- "two"
--
-- uses @'noneOf' 'separators'@
--
anyWord :: Parsec Token
anyWord = (spaced . Parsec.many1 $ noneOf separators) <?> "a word"

-- | @separators = ",;:. \\t\\n\\r"@
--
-- assumes the @Recognize@r should return only letters.
--
separators :: String
separators = ",;:. \t\n\r"

-- | let's you treat words (i.e. 'String's) as "tokens"; when your
-- 'Stream' is a 'String', and thus your real tokens are 'Char's.
--
-- >>> parse (word "word") "word"
-- "word"
--
-- >>> parse (word "word") "  word  "
-- "word"
--
-- >>> parse (word "word") "drow"
-- *** Exception: "drow" (line 1, column 1):
-- unexpected "d"
-- expecting a space or "word"
--
word :: String -> Parsec Token
word = spaced . string

-- |
--
spaced :: Parsec a -> Parsec a
spaced = between (many space) (many space)

-- | matches only the space character
--
-- shadows 'Parsec.space'.
space :: Parsec Char
space = char ' ' <?> "a space"

-- | skips zero or more of:
--
-- * any Unicode space character
-- * the control characters: tab, newline, carriage return, line feed,
-- vertical tab
--
-- >>> parse (whitespace *> eof) "　\t\n\r\f\v "
--
-- >>> parse (whitespace *> eof) "tnrfv"
-- *** Exception: "tnrfv" (line 1, column 1):
-- unexpected 't'
-- expecting white space or end of input
--
whitespace :: Parsec ()
whitespace = Parsec.spaces

-- | as 'Text.Parsec.zeroParser' is the "additive" identity for
-- 'Text.Parsec.<|>' which always fails; so 'parserUnit' is the
-- "multiplicative" (right-)identity for 'Text.Parsec.<*'
-- (and left-identity for 'Text.Parsec.*>') which always
-- succeeds, without consuming anything, before being discarded:
--
-- @p <* parserUnit = p@
--
-- assuming ParsecT satisfies the Applicative laws in
-- <http://hackage.haskell.org/package/base-4.7.0.2/docs/Control-Applicative.html>,
-- here is the proof
-- (I'm just having fun practicing equational reasoning, you don't
-- need to read it, but I do assume this law in my code):
--
-- @
-- p \<* parserUnit
-- -- definition: parserUnit = pure ()
-- p \<* pure ()
-- -- definition: u \<* v = pure const \<*> u \<*> v
-- pure const \<*> p \<*> pure ()
-- -- left-associativity of \<*>: infixl 4 \<*>
-- (pure const \<*> p) \<*> pure ()
-- -- Applicative interchange: u \<*> pure y = pure ($ y) \<*> u
-- pure ($ ()) \<*> (pure const \<*> p)
-- -- Applicative composition: pure (.) \<*> u \<*> v \<*> w = u \<*> (v \<*> w)
-- pure (.) \<*> pure ($ ()) \<*> pure const \<*> p
-- -- left-associativity of \<*>: infixl 4 \<*>
-- ((pure (.) \<*> pure ($ ())) \<*> pure const) \<*> p
-- -- Applicative homomorphism: pure f \<*> pure x = pure (f x)
-- (pure ((.) ($ ())) \<*> pure const) \<*> p
-- -- definition: (.) f g x = f (g x)
-- (pure (\\g x -> ($ ()) (g x))) \<*> pure const) \<*> p
-- -- definition: ($) f x = f x
-- (pure (\\g x -> ((g x) ())) \<*> pure const) \<*> p
-- -- left-associativity of partial application: (f x y) = (f x) y
-- (pure (\\g x -> g x ()) \<*> pure const) \<*> p
-- -- Applicative homomorphism: pure f \<*> pure x = pure (f x)
-- pure ((\\g x -> g x ()) const) \<*> p
-- -- β-reduction
-- pure (\\x -> const x ()) \<*> p
-- -- definition: const x y = x
-- pure (\\x -> x) \<*> p
-- -- definition: id x = x
-- pure id \<*> p
-- -- Applicative identity: pure id \<*> v = v
-- p
-- @
--
--
parserUnit :: Parsec ()
parserUnit = pure ()

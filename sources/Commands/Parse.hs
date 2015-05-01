{-# LANGUAGE GADTs, NamedFieldPuns, RankNTypes #-}
module Commands.Parse where
import Commands.Command.Types        ()
import Commands.Etc
import Commands.Grammar
import Commands.Grammar.Types
import Commands.Parse.Types
import Commands.Parsec

import Control.Alternative.Free.Tree
import Control.Applicative
import Control.Exception.Lens        (handler, _ErrorCall)
import Control.Lens
import Control.Monad                 ((>=>))
import Control.Monad.Catch           (Handler, SomeException (..))
import Data.Foldable                 (asum)
import Data.Typeable                 (cast)


sparser :: GrammaticalSymbol a -> Parser a
sparser s context = symbol (flip wparser $ context) (flip gparser $ context) s

wparser :: Word -> Parser a
wparser (Word w) _ = try (word w) *> pure undefined -- TODO make safe

gparser :: Grammar a -> Parser a
gparser grammar context = (grammar ^. gramParser) context <?> showLHS (grammar ^. gramLHS)

-- | build a parser from a right-hand side.
--
--
-- Implementation Detail: we use 'foldr' (not @foldl@, nor @foldr'@) to support infinitely many alternatives.
--
--
--
-- like most Haskell functions, the method '<|>' (instantiated to the Parsec Applicative) is non-strict:
--
-- >>> parse (word "hello" <|> undefined) "hello"
-- "hello"
--
-- "right-associated" means that the topmost "thing" is the leftmost. e.g. the expression:
--
-- @
-- 1:2:[]
-- @
--
-- associates as:
--
-- @
--  (:)
--  / \\
-- 1  (:)
--    / \\
--   2  []
-- @
--
-- so when our "constructor" is non-strict (like @:@, unlike @+@) and "generative" (or "sufficiently non-strict", I don't know the right jargon for what I mean), a consumer can consume one "thing" at a time.
--
-- e.g. 'parse' lazily consumes @p '<|>' (q '<|>' ...)@: if @p@ fails it tries @q@; if @q@ succeeds it doesn't try @...@; so even if @...@ is bottom (e.g. doesn't terminate), that's okay.
--
-- that's why we right-associate '<|>' (whose @infixl@ doesn't matter, as I think (?) the '<|>' method satisfies associativity anyway) with 'foldr'.
--
--
--
-- always returns a 'Parsec' wrapped in 'try'
rparser :: RHS a -> Parser a
rparser (Pure a) _ = pure a
rparser (Many rs) context = try (asum ps)
 where
 ps = fmap (flip rparser $ context) rs
rparser (fs `App` x) context = try (p <*> q)
 where
 p = rparser fs (Some q)
 q = sparser x  context
rparser (fs :<*> xs) context = try (p <*> q)
 where
 p = rparser fs (Some q)
 q = rparser xs context

-- TODO rewrite with generic function on free alternatives? but right-context-sensitive Parser is not Applicative, violating composition.

{-

grammar :: Grammar x
rs :: Alt Grammar (x -> a)

traverse :: (RHS a -> Reader Context (Parsec a)) -> [RHS a] -> Reader Context [Parsec a]

q :: Parsec x
fs :: [Parsec (x -> a)]


(<*>) :: [Parsec x -> Parsec a] -> [Parsec x] -> [Parsec a]
(<*>) _ :: f a -> f b

ps :: [Parsec a]
p :: Parsec a

-}

-- TODO strip whitespace from either end and when more than one space
parsing :: Parser a -> String -> Possibly a
parsing sp s = parse (fp <* eof) s
 where fp = sp (Some eof)

-- | see <https://hackage.haskell.org/package/lens-4.7/docs/Control-Exception-Lens.html#g:6 Control.Exception.Lens>
_ParseError :: Prism' SomeException ParseError
_ParseError = prism SomeException $ \(SomeException e) -> case cast e of
 Nothing -> Left (SomeException e) -- wrong type (preserve)
 Just a  -> Right a                -- Right type (project)

parseHandlers :: [Handler IO ()]
parseHandlers =
 [ handler _ParseError $ print >=> const (putStrLn "")
 , handler _ErrorCall  $ print
 ]

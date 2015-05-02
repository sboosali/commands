{-# LANGUAGE GADTs, NamedFieldPuns, RankNTypes #-}
module Commands.Parse where
import Commands.Command.Types        ()
import Commands.Etc
import Commands.Grammar
import Commands.Grammar.Types
import Commands.Parse.Types
import Commands.Parsec
import Control.Alternative.Free.Tree

import Control.Exception.Lens        (handler, _ErrorCall)
import Control.Lens
import Control.Monad.Catch           (SomeException (..))

import Control.Applicative
import Control.Exception             (Handler)
import Control.Monad                 ((>=>))
import Data.Foldable                 (asum)


sparser :: GrammaticalSymbol a -> Parser a
sparser = symbol wparser gparser

wparser :: Word -> Parser a
wparser (Word w) = freeParser $
 try (word w) *> pure undefined -- TODO make safe

gparser :: Grammar a -> Parser a
gparser g = SensitiveParser $ \context ->
 runSensitiveParser sp context <?> showLHS l
 where
 sp = (g^.gramParser)
 l  = (g^.gramLHS)

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
-- TODO the parser returned is wrapped in a 'try'
rparser :: RHS a -> Parser a
rparser (Pure a)  = freeParser $ pure a
rparser (Many fs) = SensitiveParser $ \context -> try (asum $ ((flip runSensitiveParser) context) <$> (rparser <$> fs))
rparser (f `App` x) = SensitiveParser $ \context -> let
 p = runSensitiveParser (rparser f) (Some q)
 q = runSensitiveParser (sparser x) context
 in
 try (p <*> q)
rparser (f :<*> g) = SensitiveParser $ \context -> let
 p = runSensitiveParser (rparser f) (Some q)
 q = runSensitiveParser (rparser g) context
 in
 try (p <*> q)

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


parsing :: Parser a -> String -> Possibly a
parsing sp s = parse (fp <* eof) s
 where fp = runSensitiveParser sp (Some eof) -- a context-free parser from a context-sensitive parser
-- TODO strip whitespace from either end and when more than one space?

handleParse :: Show a => Parser a -> String -> IO ()
handleParse p s = handles parseHandlers $ do
 x <- p `parsing` s
 print x

-- | "Control.Exception.Handler", not "Control.Monad.Catch.Handler" or "Control.Monad.Error.Lens.Handler".
parseHandlers :: [Handler ()]
parseHandlers =
 [ handler _ParseError $ print >=> const (putStrLn "")
 , handler _ErrorCall  $ print
 ]

-- | see <https://hackage.haskell.org/package/lens-4.7/docs/Control-Exception-Lens.html#g:6 Control.Exception.Lens>
_ParseError :: Prism' SomeException ParseError
_ParseError = prismException


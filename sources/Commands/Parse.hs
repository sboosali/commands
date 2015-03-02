{-# LANGUAGE GADTs, RankNTypes #-}
module Commands.Parse where
import Commands.Etc
import Commands.Grammar.Types
import Commands.Parse.Types
import Commands.Parsec
import Control.Alternative.Free.Johansen
import Control.Applicative
-- import Control.Monad.Reader
-- import Data.Foldable                         (foldr')
-- import Data.Traversable
-- import Commands.Munging       (unCamelCase)
import Control.Exception.Lens            (handler, _ErrorCall)
import Control.Lens
import Control.Monad.Catch               (Handler, SomeException (..), catches)
import Data.Foldable                     (foldr)
import Prelude                           hiding (foldr)
-- import Data.Functor.Constant
-- import Data.Functor.Product
-- import Data.List              (intercalate)
import Data.Typeable                     (cast)


-- | build a parser from a grammar.
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
gparser :: Grammar a -> SensitiveParser a
gparser (Terminal s) _ = try (word s) *> pure undefined -- TODO make safe
gparser (NonTerminal l (Alt rs)) context = try (p <?> show l)
 where
 ps = fmap (flip rparser $ context) rs
 p = foldr (<|>) empty ps
-- TODO breadth-first foldr

-- | build a parser from a right-hand side.
--
rparser :: RHS a -> SensitiveParser a
rparser (Pure a) _ = pure a
rparser (Alt rs `App` g) context = try (p <*> q)
 where
 q = (flip gparser) context g
 ps = fmap (flip rparser $ Some q) rs
 p = foldr (<|>) empty ps

parses :: Grammar a -> String -> Possibly a
parses g s = parse (p <* eof) s
 where p = (gparser g) (Some eof)

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

-- | see <https://hackage.haskell.org/package/lens-4.7/docs/Control-Exception-Lens.html#g:6 Control.Exception.Lens>
_ParseError :: Prism' SomeException ParseError
_ParseError = prism SomeException $ \(SomeException e) -> case cast e of
 Nothing -> Left (SomeException e) -- wrong type (preserve)
 Just a  -> Right a                -- Right type (project)

parseHandlers :: [Handler IO ()]
parseHandlers =
 [ handler _ParseError print
 , handler _ErrorCall print
 ]

handleParse :: Show a => Grammar a -> String -> IO ()
handleParse p s = do
 (print =<< (p `parses` s)) `catches` parseHandlers
 putStrLn ""


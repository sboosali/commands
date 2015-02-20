{-# LANGUAGE GADTs, RankNTypes #-}
module Commands.Parse where
import Commands.Etc
import Commands.Grammar.Types
import Commands.Parse.Types
import Commands.Parsec
import Control.Alternative.Free.Johansen
import Control.Applicative
import Control.Monad.Reader
import Data.List                         (foldl')
import Data.Traversable
-- import Commands.Munging       (unCamelCase)
-- import Control.Exception.Lens (handler)
-- import Control.Lens           hiding (Context)
-- import Control.Monad.Catch    (Handler, SomeException (..), catches)
-- import Data.Foldable          (asum)
-- import Data.Functor.Constant
-- import Data.Functor.Product
-- import Data.List              (intercalate)
-- import Data.Typeable          (cast)


gparser :: Grammar a -> SensitiveParser a
gparser (Terminal s) = return $ string s *> undefined
gparser (NonTerminal l (Alt rs)) = do
 ps <- traverse rparser rs
 let p = foldl' (<|>) empty ps
 return (p <?> show l)


rparser :: RHS a -> SensitiveParser a
rparser (Pure a) = return . pure $ a
rparser (Alt rs `App` g) = do
 q <- gparser g
 ps <- with (Some q) $ traverse rparser rs  -- recur on the left, with the parser from the right
 let p = foldl' (<|>) empty ps
 return $ p <*> q               -- run the parser from the left, before the parser from the right

with :: Monad m => r -> ReaderT r m a -> ReaderT r m a
with = local . const

{- |

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

parses :: Grammar a -> String -> Possibly a
parses g s = parse p s
 where p = runReader (gparser g) (Some eof)

-- _ParseError :: Prism' SomeException ParseError
-- _ParseError = prism SomeException $ \(SomeException e) ->
--  case cast e of
--   Nothing -> Left (SomeException e)
--   Just a  -> Right a

-- parseHandlers :: [Handler IO ()]
-- parseHandlers =
--  [ handler _ParseError print
--  ]

-- handleParse :: Show a => IO a -> IO ()
-- handleParse parseAction = do
--  (print =<< parseAction) `catches` parseHandlers
--  putStrLn ""

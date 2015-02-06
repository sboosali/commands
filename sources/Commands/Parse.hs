{-# LANGUAGE RankNTypes #-}
module Commands.Parse where
-- import Commands.Etc
-- import Commands.Munging       (unCamelCase)
-- import Commands.Parse.Types
-- import Commands.Parsec
-- import Control.Applicative
-- import Control.Exception.Lens (handler)
-- import Control.Lens           hiding (Context)
-- import Control.Monad.Catch    (Handler, SomeException (..), catches)
-- import Data.Foldable          (asum)
-- import Data.Functor.Constant
-- import Data.Functor.Product
-- import Data.List              (intercalate)
-- import Data.Typeable          (cast)



-- gparser :: Grammar a -> SensitiveParser a
-- gparser = _

-- rparser :: RHS a -> SensitiveParser a
-- rparser = _

-- parses :: Grammar a -> String -> Possibly a
-- parses (SensitiveParser sp) = parse $ sp (Some eof)

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

{-# LANGUAGE NamedFieldPuns, RankNTypes, ScopedTypeVariables #-}
module Commands.Command where
import           Commands.Etc
import           Commands.Frontends.Dragon13
import           Commands.Frontends.Dragon13.Render
import           Commands.Frontends.Dragon13.Types
import           Commands.Grammar
import           Commands.Grammar.Types
import           Commands.Parse
import           Commands.Parse.Types
-- import           Commands.Frontends.Dragon13.Text
-- import           Control.Alternative.Free.Tree

-- import           Control.Applicative
import           Control.Monad.Catch                (SomeException (..),
                                                     catches)
import           Data.Bifunctor                     (bimap)
import           Data.Foldable                      (asum)
import           Data.Proxy
import qualified Data.Text.Lazy                     as T
import           Data.Typeable                      (Typeable)
import           Language.Haskell.TH.Syntax         (Name)


serialized :: Command x -> Either [SomeException] T.Text
serialized Command{_grammar} = serialize $ bimap T.pack T.pack $ _grammar

parses :: Command a -> String -> Possibly a
parses Command{_parser} = parsing _parser

handleParse :: Show a => Command a -> String -> IO ()
handleParse command s = do
 (print =<< (command `parses` s)) `catches` parseHandlers


-- | a default 'Command' for simple ADTs.
--
-- with 'Enum' ADTs, we can get the "edit only once" property: edit the @data@ definition, then 'terminal' builds the 'Rule', and then the functions on 'Rule's build the 'Parser's and 'DNSGrammar's. without TemplateHaskell.
--
-- the 'LHS' comes from the type, not the term (avoiding TemplateHaskell). other 'Command's can always be defined with an LHS that comes from the term, e.g. with '<=>'.
--
--
defaultCommand :: forall a. (Typeable a, Enum a, Show a) => Command a
defaultCommand = genericCommand
 (LHS . guiOf $ (Proxy :: Proxy a))
 (asum . fmap con $ constructors)

-- |
genericCommand :: LHS -> RHS a -> Command a
genericCommand l r = Command l g p
 where
 -- g = bimap T.pack T.pack $ renderRHS r
 g = renderRule (Rule l r)
 p = rparser r

-- | builds a special 'Command' directly, not indirectly via 'Rule'.
specialCommand :: Name -> DNSGrammar String String -> Parser a -> Command a
specialCommand name g p = Command l g p
 where
 l = unsafeLHSFromName name


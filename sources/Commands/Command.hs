{-# LANGUAGE NamedFieldPuns, RankNTypes, ScopedTypeVariables #-}
module Commands.Command where
import           Commands.Etc
import           Commands.Frontends.Dragon13
import           Commands.Frontends.Dragon13.Optimize
import           Commands.Frontends.Dragon13.Render
import           Commands.Frontends.Dragon13.Types
import           Commands.Grammar
import           Commands.Grammar.Types
import           Commands.Parse
import           Commands.Parse.Types
-- import           Commands.Frontends.Dragon13.Text
-- import           Control.Alternative.Free.Tree

-- import           Control.Applicative
import           Control.Monad.Catch                  (SomeException (..),
                                                       catches)
import           Data.Bifunctor                       (second)
import           Data.Foldable                        (asum)
import           Data.Proxy
import qualified Data.Text.Lazy                       as T
import           Data.Typeable                        (Typeable)
import           GHC.Generics                         (Generic)
import           Language.Haskell.TH.Syntax           (Name)


serialized :: Command x -> Either [SomeException] T.Text
serialized Command{_grammar} = serialize $ second T.pack $ optimizeGrammar $ _grammar

parses :: Command a -> String -> Possibly a
parses Command{_parser} = parsing _parser

handleParse :: Show a => Command a -> String -> IO ()
handleParse command s = do
 (print =<< (command `parses` s)) `catches` parseHandlers


-- |
genericCommand :: LHS -> RHS a -> Command a
genericCommand l r = Command l g p
 where
 -- g = bimap T.pack T.pack $ renderRHS r
 g = renderRule (Rule l r)
 p = rparser r

-- | builds a special 'Command' directly, not indirectly via 'Rule'.
--
-- warning: partial function:
--
-- * match fails on non-global 'Name's
specialCommand :: Name -> DNSGrammar DNSCommandName DNSCommandToken -> Parser a -> Command a
specialCommand name g p = Command l g p
 where
 Just l = lhsFromName name

-- | a default 'Command' for simple ADTs.
--
-- with 'Enum' ADTs, we can get the "edit only once" property: edit the @data@ definition, then 'terminal' builds the 'Rule', and then the functions on 'Rule's build the 'Parser's and 'DNSGrammar's. without TemplateHaskell.
--
-- the 'LHS' comes from the type, not the term (avoiding TemplateHaskell). other 'Command's can always be defined with an LHS that comes from the term, e.g. with '<=>'.
--
--
enumCommand :: forall a. (Typeable a, Enum a, Show a) => Command a
enumCommand = genericCommand
 (lhsOfType (Proxy :: Proxy a))
 (asum . fmap con $ constructors)

-- | a default 'Command' for simple ADTs.
--
-- detects the type name in a constructor name (as a
-- prefix/infix/suffix) and elides the affix.
--
-- useful when you want your @commands@-DSL terminals to be
-- unqualified (for convenience), but you want your Haskell
-- identifiers to be qualified (to avoid conflicts). e.g.:
--
-- >>> :set -XDeriveDataTypeable
-- >>> data Button = LeftButton | ButtonMiddleButton | ButtonRight deriving (Show,Eq,Enum,Typeable)
-- >>> let button = qualifiedCommand :: Command Button
-- >>> getWords . _grammar $ button
-- ["left","middle","right"]
--
-- (the qualification is exaggerated to show the filtering behavior:
-- it's consistent in idiomatic declarations).
--
-- we didn't define @data Button = Left | Middle | Right@ because it
-- conflicts with 'Either', but the derived grammar is identical.
--
--
--
--
qualifiedCommand :: forall a. (Typeable a, Enum a, Show a) => Command a
qualifiedCommand = genericCommand
 (lhsOfType (Proxy :: Proxy a))
 (asum . fmap (qualifiedCon occ) $ constructors)
 where
 GUI _ _ (Identifier occ) = guiOf (Proxy :: Proxy a)

-- | a default 'Command' for 'String' @newtype@s.
--
-- e.g. @newtype Place = Place String deriving (Show,Eq)@
--
vocabularyCommand :: (Generic a) => [String] -> RHS a
vocabularyCommand = undefined vocabulary
-- (Newtype a String) => [String] -> RHS a


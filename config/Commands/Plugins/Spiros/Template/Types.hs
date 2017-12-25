{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, TypeFamilies, ViewPatterns, FlexibleContexts #-}
module Commands.Plugins.Spiros.Template.Types where
-- import Commands.Plugins.Spiros.Extra (insertByClipboard)
import Commands.Plugins.Spiros.Extra.Types

import Commands.Backends.Workflow as W

import Data.Monoid.Split

-- import qualified Data.List as List
import Control.Monad (replicateM_)
import           GHC.Exts                          (IsString (..)) -- , IsList (..))

import Prelude.Spiros hiding (rstrip,lstrip)
import Prelude()

{- | a simple monoid that can hold the metadata of a cursor position. you can then can set the position of the cursor after inserting a template.

can be used with @-XQuasiQuotes@ ('qc' supports interpolation).

see "Commands.Plugins.Spiros.Template.haddockTemplate" for example.

TODO...

Template ~ [Either (First ()) String)]

(String, First (), String) ~ (String, String)

:: [Either (First ()) String] -> (String, String)

'Split' keeps the rightmost divider:

>>> import Data.Monoid.Split
>>> M "a" <> ("b" :| "c") <>  ("d" :| "e")
"abcd" :| "e"

-}
-- data Template
--  = TemplateCursor
--  | TemplateText String
--  | TemplateList [Template]
--  deriving (Show,Read,Eq,Ord,Generic,Data,NFData)

newtype Template = Template { getTemplate :: Split String }
  deriving (Show,Read,Eq,Generic,Data,Semigroup,Monoid) -- TODO Hashable, Ord, NFData

{- | when constructed with 'mappend', a @Template@ is always flat (see 'flattenTemplate').

@
 'mempty' = 'TemplateList' []
@

-}
-- instance Monoid Template where
--  mempty = TemplateList []
--  mappend x y = TemplateList (mappend (toTemplateList x) (toTemplateList y))

{-|

@
 'fromString' ~ 'M'
@

-}
instance IsString Template where
 fromString = M > Template

{-|

@
 'fromList' = 'fromTemplateList'
 'toList'   = 'toTemplateList'
@

-}

-- instance IsList Template where
--  type Item Template = Template
--  fromList = fromTemplateList
--  toList = toTemplateList
--
-- toTemplateList :: Template -> [Template]
-- toTemplateList = \case
--  TemplateList ts -> ts
--  t -> [t]
--
-- fromTemplateList :: [Template] -> Template
-- fromTemplateList = TemplateList

-- | @getTemplate > 'unsplit'@
fromTemplate :: Template -> String
fromTemplate = getTemplate > unsplit

-- ================================================================ --

{-| munges the template, making it 'insert'able.

strips one leading newline and one trailing newline.

increases readability of quasi-quotes, e.g.:

@
haddockTemplate_2 = ['qc'|
\{-| {'cursor'}

-}
|]
@

rather than:

@
haddockTemplate_3 = ['qc'|\{-| {'cursor'}

-}|]
@

becoming:

@
Template {getTemplate = "\n{-| " :| "\n\n-}\n"}
@

if you want that leading/trailing white space, just add "extra" whitespace, or use a string literal.

-}
mungeTemplate :: Template -> (String, String)
mungeTemplate template = (before, after)
 where
 before = (lstrip) beforeTemplate
 after  = (rstrip) afterTemplate
 (beforeTemplate, afterTemplate) = splitTemplateByCursor template
 rstrip = reverse.lstrip.reverse -- TODO case on  nonempty,  last, check, init
 lstrip = \case
  ('\n':xs) -> xs
  xs -> xs

{- | input should have zero or one 'TemplateCursor'(s). splits on the first, when multiple.

outputs should have zero 'TemplateCursor'(s).

-}
splitTemplateByCursor :: Template -> (String, String)
splitTemplateByCursor = getTemplate > \case
  M x    -> (x, "") -- no cursor
  x :| y -> (x, y)

{- | @cursor ~ 'split'@

there is always zero @('Template' ('M' ...))@
or one @('Template' (... ':|' ...))@ cursor.

-}
cursor :: Template
cursor = Template split

-- ================================================================ --
-- workflow stuff

{- |
strips one leading newline and one trailing newline,
which increases the readability of quasiquotes.

-}
insertTemplate :: MonadWorkflow m => Template -> m ()
insertTemplate template = do
 let (before, after) = mungeTemplate template

 -- -- when inserted not by clipboard, it's
 -- -- (a) harder to undo and
 -- -- (b) triggers functions, like "import" opens the mini buffer
 -- insertByClipboard before
 -- insertByClipboard after
 -- replicateM_ (length after) (W.press "<left>")

 -- when inserted not by clipboard, it's
 -- (a) harder to undo and
 -- (b) triggers functions, like "import" opens the mini buffer
 W.insert before
 W.insert after
 replicateM_ (length after) moveLeft
 where
 moveLeft = W.press "<left>"

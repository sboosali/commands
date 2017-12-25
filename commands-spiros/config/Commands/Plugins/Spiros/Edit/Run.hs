{-# LANGUAGE LambdaCase, FlexibleContexts #-}
module Commands.Plugins.Spiros.Edit.Run where 
import Commands.Plugins.Spiros.Edit.Types
import Commands.Plugins.Spiros.Emacs
import           Commands.Plugins.Spiros.Extra

import Commands.Backends.Workflow as W


rankMove :: Move -> Int
rankMove _m = 0

rankEdit :: Edit -> Int
rankEdit (Edit a s r) = (sum . fmap fromEnum) [a /= defaultAction, s /= defaultSlice, r /= defaultRegion]
-- defaults have lower rank
-- disambiguates "cop fuss line" into [cop fuss line], not [cop] [fuss line] which comes first in the order 

runMove :: MonadWorkflow m => Move -> m() 
runMove = moveEmacs

-- the indirection (i.e. @data 'Move'@, not just a @String@) makes it easy to reinterpret in many ways (e.g. moveEmacs, moveIntelliJ, moveChromd , etc).
moveEmacs :: MonadWorkflow m => Move -> m ()
moveEmacs = \case

 Move Left_ Character  -> press "C-b"
 Move Right_ Character -> press "C-f"

 Move Left_ Word_      -> press "M-b"
 Move Right_ Word_     -> press "M-f"

 Move Left_ Group      -> press "C-M-b"
 Move Right_ Group     -> press "C-M-f"

 Move Up_ Line         -> press "C-p"
 Move Down_ Line       -> press "C-n"

 Move Up_ Block        -> press "C-<up>"
 Move Down_ Block      -> press "C-<down>"

 Move Up_ Screen       -> runEmacs "scroll-up-command"
 Move Down_ Screen     -> press "C-v"

 Move Up_ Page         -> runEmacs "backward-page"
 Move Down_ Page       -> runEmacs "forward-page"

 MoveTo Beginning Line       -> press "C-a"
 MoveTo Ending    Line       -> press "C-e"

 MoveTo Beginning Everything -> press "M-<up>"
 MoveTo Ending    Everything -> press "M-<down>"

 -- Move -> press
 -- MoveTo -> press
 _ -> nothing

-- gets the given region of text from Emacs
selected :: MonadWorkflow m => Slice -> Region -> m String
selected s r = do
 -- editEmacs (Edit Select s r)
 select r s
 copy

select :: MonadWorkflow m => Region -> Slice -> m ()
select That = \case
 _ -> nothing     -- (should be) already selected
-- select Character = \case
--  _ -> nothing
select r = \case
 Whole     -> beg_of r >> mark >> end_of r
 Backwards -> mark >> beg_of r
 Forwards  -> mark >> end_of r

{-

idempotent means

idempotent means unchainable.
instead of [3 select word], how about [select 3 word]
where the first selection is idempotent, and the next two Move Right.
In Emacs, this preserves the mark.



-}
-- | should be idempotent (in Emacs, not Haskell).
beg_of :: MonadWorkflow m => Region -> m ()
beg_of = \case
 -- runEmacs
 That       -> evalEmacs "(goto-char (region-beginning))"
 Character  -> nothing
 Word_      -> evalEmacs "(beginning-of-thing 'word)"
 Group      -> evalEmacs "(beginning-of-thing 'list)"
 Line       -> press "C-a"
 Block      -> evalEmacs "(beginning-of-thing 'block)"
 Page       -> evalEmacs "(beginning-of-thing 'page)"
 Screen     -> evalEmacs "(goto-char (window-start))"
 Everything -> runEmacs "beginning-of-buffer"
 _          -> nothing

-- | should be idempotent (in Emacs, not Haskell).
end_of :: MonadWorkflow m => Region -> m ()
end_of = \case
 -- runEmacs
 That       -> evalEmacs "(goto-char (region-end))"
 Character  -> nothing          -- [press C f] is not idempotent, but [nothing] fails on [beg_of r >> mark >> end_of r]
 Word_      -> evalEmacs "(end-of-thing 'word)"
 Group      -> evalEmacs "(end-of-thing 'list)"
 Line       -> press "C-e"
 Block      -> evalEmacs "(end-of-thing 'block)" -- non-standard: expects forward-block
 Page       -> evalEmacs "(end-of-thing 'page)"
 Screen     -> evalEmacs "(goto-char (window-end))"
 Everything -> runEmacs "end-of-buffer"
 _          -> nothing

runEdit :: MonadWorkflow m => Edit -> m() 
runEdit = editEmacs

-- | vim's composeability would keep the number of cases linear (not quadratic in 'Action's times 'Region's).
-- in Emacs, we can use <http://www.emacswiki.org/emacs/ThingAtPoint thingatpt.el>.
editEmacs :: MonadWorkflow m => Edit -> m ()
editEmacs = \case

 Edit Select Whole Line -> do -- special behavior
  select Line Whole
  press "<right>"
 Edit Select _ Character -> do -- special behavior
  mark
  press "<right>" 
 Edit Select s r -> do
  select r s  -- generic behavior
  activate_mark
 -- some Regions need a { press right } for idempotency of their beg_of/end_of

 Edit Google s r -> do
  google =<< selected s r

 Edit Delete s r -> do
  select r s
  press "<del>" 

 Edit Copy s r -> do
  select r s
  press "M-c"                     -- like Cua-mode for Mac

 Edit Cut s r -> do
  select r s
  press "M-x"                     -- like Cua-mode for Mac

 Edit Transpose _ Character -> press "C-t"
 Edit Transpose _ Word_     -> press "M-t"
 Edit Transpose _ Group     -> press "C-M-t"
 Edit Transpose _ Line      -> press "C-x-t"
 Edit Transpose _ Block     -> runEmacs "transpose-block" -- nonstandard
 -- Edit Transpose _ ->

 -- That
 -- Character
 -- Word_
 -- Token
 -- Group
 -- Line
 -- Rectangle
 -- Block
 -- Page
 -- Screen
 -- Everything
 -- Definition
 -- Function_
 -- Reference
 -- Structure

 _ -> nothing


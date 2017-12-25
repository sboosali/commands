{-# LANGUAGE DeriveAnyClass #-}
module Commands.Plugins.Spiros.Edit.Types where 
import Commands.Plugins.Spiros.Extra.Types 


data Move
 = Move   Direction Region      -- ^ 
 | MoveTo Endpoint  Region      -- ^ idempotent 
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData)

-- | orthogonal directions in three-dimensional space. @... <=> Up_ <$ "up" <|> ...@
data Direction = Up_ | Down_ | Left_ | Right_ | In_ | Out_  deriving (Show,Read,Eq,Ord,Bounded,Enum,Generic,Data,NFData)

-- | Slice and Direction both have too many values.
data Endpoint = Beginning | Ending deriving (Show,Read,Eq,Ord,Bounded,Enum,Generic,Data,NFData)

{- | slice the region between the cursor and the 'Slice'. induces a string.
-}
data Slice = Whole | Backwards | Forwards  deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData)

data Edit = Edit Action Slice Region deriving (Show,Read,Eq,Ord,Generic,Data,NFData)

data Action
 = Select                       -- read-only.
 | Copy                         -- read-only.
 | Cut                          -- read/write.
 | Delete                       -- read/write.
 | Transpose                    -- read/write.
 | Google                       -- read-only.
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData)


data Region
 = That

 | Character
 | Word_                        -- ^ e.g. @"camelCase"@, @"lisp-case"@, @"snake_case"@ are all two 'Word_'s
 | Token                        -- ^ e.g. @"camelCase"@, @"lisp-case"@, @"snake_case"@ are all one 'Token's
 | Group                        -- ^ 'Bracket's delimit 'Group's (e.g. @"(...)"@ or @"<...>"@ or @"[|...|]"@)
 | Line
 | Rectangle
 | Block
 | Page
 | Screen
 | Everything

 | Definition
 | Function_
 | Reference
 | Structure
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData)

defaultAction :: Action 
defaultAction = Select 

defaultSlice :: Slice 
defaultSlice = Whole

defaultRegion :: Region 
defaultRegion = That


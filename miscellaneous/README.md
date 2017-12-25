
configuring speech recognition engines (Dragon NaturallySpeaking being the most configurable), such a voice command calls back to arbitrary Haskell when the recognition is matched. 

e.g. https://github.com/sboosali/commands-spiros/blob/windows/config/Commands/Plugins/Spiros/Edit.hs

the core type is an Applicative for defining grammars, so that you can simultaneously generate a parser (to parse recognitions) and a serialized grammar that's consistent with it (to bias recognitions when loaded by the speech engine).
For example, given this grammar

data Action = Copy | Google
 deriving (Show, Enum, Bounded)

data Region = Word | Line | Selection | Clipboard
 deriving (Show, Enum, Bounded)

data Phrase 
 = KnownPhrase  Region 
 | SpokenPhrase [String]
 deriving (Show)

data Command = Command Action Phrase
 deriving (Show)
 -- the root of our grammar

-- :: RHS Region
gRegion = "<region>"
 <=> Word      $> "this word" 
 <|> Line      $> "this line" 
 <|> Selection $> "that" 
 <|> Clipboard $> "the clipboard" 

-- :: RHS Action
gAction = "<action>"
 <=> defaultGrammarWith (fmap toLower)

-- defaultGrammarWith :: (Show a, Enum a, Bounded a) => (String -> String) -> RHS a
-- it enumerates the constructors and munges them

{- equivalent to

    Copy      $> "copy" 
<|> Google    $> "google" 

-}

-- :: RHS Phrase
gPhrase = "<phrase>"
 <=> KnownPhrase  <$> gRegion 
 <|> SpokenPhrase <$> gDictation

-- gDictation :: RHS [String]
-- gDictation is a built-in, it represents arbitrary dictation, i.e. one or more words from the English lexicon

gCommand = "<command>" <=> gCommand <$> gAction <*> gPhrase

-- note, we could also just use strings directly, since there is a little boilerplate for defining the Command type and its descendants, but I prefer going FULLHASKELL (to get the usual benefits from validation, like debugability)

And these command handlers:

-- execute a command
eCommand :: Command -> IO ()
eCommand (Command Copy   phrase) = do
 selectPhrase
 defaultCopy
eCommand (Command Google phrase) = do
 s <- getPhrase
 google s

eAction = \case
 Copy | Google

eRegion = \case
 Word | Line | Selection | Clipboard

getPhrase = \case
 KnownPhrase  r  -> getRegion r
 SpokenPhrase ws -> return ws

-- highlight or access
getRegion = \case
 Clipboard -> do
               -- directly access the clipboard
               getClipboard
 Selection -> do
               -- first copy whatever is currently highlighted
               copy
 Word      -> do
               -- first highlight the current word
               press "A-b"   -- go to the start
               press "S-A-f" -- drag across to the end
               copy
 Line      -> do
               -- first highlight the current line
               press "C-a"   -- go to the statt
               press "S-C-e" -- drag across to the end             
               copy
 
copy = defaultCopy

-- copy whatever is currently highlighted
-- uses the default keyboard shortcut of ctrl+c
-- the pause gives the current application enough time to process and handle the event (it should be almost instantaneous, but most apps are very slow)
defaultCopy :: IO String
defaultCopy = do
 press "C-c"
 delayMilliseconds 50
 getClipboard

google s = do
 openUrl $ "https://www.google.com/search?q=" <> urlEncode s

given bindings to the operating system's accessibility / automation features:

getClipboard :: IO String
openUrl :: String -> IO ()
-- from the `workflow` package

We get both a parser:

createParser :: RHS a -> Parser a
pCommand = createParser gCommand

>>> runParser pCommand ["google", "the", "clipboard"]
Right (Command Google (KnownPhrase Clipboard))

>>> runParser pCommand ["google", "some", "arbitrary", "dictation"]
Right (Command Google (SpokenPhrase ["some", "arbitrary", "dictation"]))

And a serialized grammar:

renderGrammar :: RHS a -> String
sCommand = renderGrammar gCommand
writeGrammar g = saveFile ".../grammar.bnf" (renderGrammar g)
 -- the grammar file is automatically reloaded by the speech engine
 -- other shims exist, like a network call or clicking through a GUI

>>> putStrLn $ renderGrammar gCommand
export <command>
<command> = <action> <phrase>
<action>  = copy | google
<phrase>  = <region> | <dictation>
<region>  = this word | this line | that | the clipboard


And we can execute the recognition:

>>> execCommand (Command Google (KnownPhrase Clipboard))
...

{- equivalent to

do
   s <- getClipboard
   openUrl $ "https://www.google.com/search?q=" <> urlEncode s

-}

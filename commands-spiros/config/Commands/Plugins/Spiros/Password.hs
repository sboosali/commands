{-| 'password's aren't logged, but otherwise behave identically to 'phrase'. 

TODO hasPassword :: Prism Root Password  

or, safer if you can only use it at the "top level" 
hasPassword :: Root -> Bool

-}
module Commands.Plugins.Spiros.Password where 

-- newtype Password = Password Phrase 

-- password = 'password <=>
--  Password <$ "password" <*> phrase 

-- runPassword (Password p) = do
--  nothing 


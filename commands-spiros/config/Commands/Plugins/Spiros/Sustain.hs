module Commands.Plugins.Spiros.Sustain where 
-- import Commands.Plugins.Spiros.Extra


-- type SustainedMonad = 

-- data Sustained = Sustained String SustainedMonad SustainedMonad

-- sustainedGrammar :: Sustained -> R SustainedMonad
-- sustainedGrammar (Sustained name begin finish) = 
--  [ "begin"  name -: begin 
--  , "finish" name -: finish 
--  ] 

-- sustainedSkipping = sustainedGrammar (Sustained "skipping" beginSkipping finishSkipping) 
--  where 
--  beginSkipping = do 
  
--  finishSkipping = do 

   
{-|


type Sustained name = Tagged name Bool

pattern On = Tagged True
pattern Off = Tagged False

sustainedGrammar :: proxy name -> R (Sustained name)
sustainedGrammar (symbolVal -> name) = vocab
 [ "begin " ++ name -: Tagged True
 , "finish " ++ name -: Tagged False
 ]

sustainedDesugar :: (MonadState (Tagged s Bool) m) => m () -> m () -> m ()
sustainedDesugar begin finish = bool begin finish =<< get 

bool :: a -> a -> Tagged s Bool -> a
bool begin finish = \case
 Tagged True -> begin
 Tagged False -> finish

pSkipping = (Proxy :: Proxy "skipping")

sustainedGrammar pSkipping 

sustainedDesugar pSkipping 


-} 

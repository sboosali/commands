{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Commands.Parse.Types where
import Commands.Etc
import Commands.Parsec (Parsec)
-- import Control.Monad.Reader


-- type Parser a = Reader Context (Parsec a)
type Parser a = Context -> (Parsec a)

type Context = Some Parsec

runParser :: Parser a -> Parsec a
runParser = undefined

-- -- | Parsers with restricted right-context-sensitivity.
-- --
-- -- 'RightContext'
-- -- existentially-quantifies its 'Parser', so the 'SensitiveParser' can
-- -- run the parser to check whether it succeeds or fails, but it can't
-- -- use any parse result. Neither should well-behaved
-- -- 'SensitiveParser's consume any tokens with the 'RightContext':
-- -- they can wrap the 'RightContext' in a call to
-- -- 'Text.Parsec.lookAhead'.
-- --
-- -- The type is isomorphic to @Compose (Reader Context) Parsec@. But
-- -- it's @Applicative@ instance is different (see
-- -- 'applySensitiveParser').
-- --
-- -- The trick is that in @Reader r f@, the environment @r@ and the
-- -- functor @f@ are independent. But here, they are dependent:
-- -- @r ~ Some f@.
-- --
-- -- Now we must prove that SensitiveParser is 'Applicative' and
-- -- 'Alternative'. see 'proof_Applicative_SensitiveParser'
-- -- and 'proof_Alternative_SensitiveParser'.
-- --
-- newtype SensitiveParser a = SensitiveParser { runSensitiveParser :: Context -> Parsec a }
--  deriving (Functor)

-- -- |
-- type Context = Some Parsec

-- instance Applicative SensitiveParser where
--  pure  = pureSensitiveParser
--  (<*>) = applySensitiveParser

-- instance Alternative SensitiveParser where
--  empty = emptySensitiveParser
--  (<|>) = alterSensitiveParser

-- -- | in @instance 'Applicative' SensitiveParser@:
-- --
-- -- @('<*>') = applySensitiveParser@
-- --
-- -- the code (beta-reduced) is just:
-- --
-- -- @
-- -- applySensitiveParser (SensitiveParser f) (SensitiveParser x) = SensitiveParser $
-- --  \\q -> f ('Some' (x q)) '<*>' x q
-- -- @
-- --
-- --
-- -- which, ignoring @newtype@ noise, means that the context of the
-- -- parser on the left (@f@) depends on the parser on the right (@x@).
-- -- @x@ depends on the context @q@, which by construction,
-- -- should itself come "from the right".
-- --
-- -- The heuristic is that
-- -- monadic parsers can be context-sensitive, while applicative parsers
-- -- must be context-free. Here, the applicative @f :: SensitiveParser
-- -- (a -> b)@ depends on the applicative @x :: SensitiveParser b@, but
-- -- not on the __result__ @result :: b@. Thus, I think that this restricted
-- -- context-sensitivity is "static" enough to be applicative. That is,
-- -- whatever the result of type @b@ is, the parser has the same "shape"
-- -- for some applicative of type @SensitiveParser b@.
-- --
-- -- for example, this context-sensitive parser:
-- --
-- -- >>> import Commands.Parse
-- -- >>> import Commands.Parsec
-- -- >>> newtype Dictation = Dictation [String] deriving (Show)
-- -- >>> let dictation = grammar "<dictation>" $ \context -> Dictation <$> anyWord `manyUntil` context
-- -- >>> data Command = ReplaceWith Dictation Dictation | Undo deriving (Show)
-- -- >>> :{
-- -- let command =  ReplaceWith <$ terminal "replace" <*> (dictation <* terminal "with") <*> dictation
-- --            <|> Undo        <$ terminal "undo"
-- -- :}
-- --
-- -- 'Commands.Parse.parses' these strings:
-- --
-- -- >>> command `parses` "replace this and that with that and this"
-- -- ReplaceWith (Dictation ["this","and","that"]) (Dictation ["that","and","this"])
-- -- >>> command `parses` "undo"
-- -- Undo
-- --
-- -- Note that the two uses of @dictation@ behave differently. The first
-- -- stops matching words at the context "one to the right" i.e.
-- -- @"with"@.
-- -- While the second stops matching words at the context "from above"
-- -- i.e. @eof@.
-- --
-- applySensitiveParser :: SensitiveParser (a -> b) -> SensitiveParser a -> SensitiveParser b
-- applySensitiveParser (SensitiveParser f) (SensitiveParser x) = SensitiveParser $ \q ->
--  let xq = x q
--  in  f (Some xq) <*> xq

-- -- | in @instance 'Applicative' SensitiveParser@:
-- --
-- -- @'pure' = pureSensitiveParser@
-- --
-- pureSensitiveParser :: a -> SensitiveParser a
-- pureSensitiveParser = SensitiveParser . const . pure

-- -- | in @instance 'Alternative' SensitiveParser@:
-- --
-- --  @('<|>') = alterSensitiveParser@
-- --
-- -- the code is just the
-- -- <http://hackage.haskell.org/package/transformers-0.4.2.0/docs/src/Control-Monad-Trans-Reader.html#ReaderT Reader instance>:
-- --
-- -- @
-- -- alterSensitiveParser (SensitiveParser x) (SensitiveParser y) = SensitiveParser $
-- --  \\q -> x q '<|>' y q
-- -- @
-- --
-- alterSensitiveParser :: SensitiveParser a -> SensitiveParser a -> SensitiveParser a
-- alterSensitiveParser (SensitiveParser x) (SensitiveParser y) = SensitiveParser $ \q ->
--  x q <|> y q

-- -- | in @instance 'Alternative' SensitiveParser@:
-- --
-- -- @'empty' = emptySensitiveParser@
-- --
-- emptySensitiveParser :: SensitiveParser a
-- emptySensitiveParser = SensitiveParser . const $ empty





-- {- | = the Alternative laws:

-- [/left-identity/]

--      @'empty' '<|>' y = y@

-- [/right-identity/]

--      @x '<|>' 'empty' = x@

-- [/associativity/]

--      @x '<|>' y '<|>' z = x '<|>' (y '<|>' z)@

-- ... are like the Monoid laws, I guess?

-- = proof

-- the proof that 'SensitiveParser' satisfies them should be
-- trivially buildable from @Parsec@'s and/or @Reader@'s proofs.

-- -}
-- proof_Alternative_SensitiveParser :: ()
-- proof_Alternative_SensitiveParser =  ()

-- {- | = The 'Applicative' laws are:

-- [/identity/]

--      @'pure' 'id' '<*>' v = v@

-- [/composition/]

--      @'pure' ('.') '<*>' u '<*>' v '<*>' w = u '<*>' (v '<*>' w)@

-- [/homomorphism/]

--      @'pure' f '<*>' 'pure' x = 'pure' (f x)@

-- [/interchange/]

--      @u '<*>' 'pure' y = 'pure' ('$' y) '<*>' u@



-- = a lemma, /left-pure/:

-- the proofs below use this lemma:

-- [/left-pure/]

--      @pure f \<*> SensitiveParser x = (SensitiveParser $ \\q -> pure f \<*> x q)@

-- proof:

-- @pure f \<*> SensitiveParser x@

--  expand: pure

-- @pureSensitiveParser f \<*> SensitiveParser x@

--  expand: pureSensitiveParser

-- @(SensitiveParser . const . pure) f \<*> SensitiveParser x@

--  expand: (.)

-- @(\y -> SensitiveParser (const (pure y))) f \<*> SensitiveParser x@

--  beta:

-- @SensitiveParser (const (pure f)) \<*> SensitiveParser x@

--  expand: \<*>

-- @applySensitiveParser (SensitiveParser (const (pure f))) (SensitiveParser x)@

--  expand: applySensitiveParser

-- @SensitiveParser $ \\q -> let xq = x q in (const (pure f)) (Some xq) \<*> xq@

--  associativity of: function application

-- @SensitiveParser $ \\q -> let xq = x q in (const (pure f) (Some xq)) \<*> xq@

--  apply: const

-- @SensitiveParser $ \\q -> let xq = x q in pure f \<*> xq@

--  beta: xq

-- @SensitiveParser $ \\q -> pure f \<*> x q@




-- = SensitiveParser satisfies /identity/:

-- @pure id \<*> v@

--  rename: SensitiveParser v' = v

-- @pure id \<*> SensitiveParser v'@

--  SensitiveParser left-pure:

-- @SensitiveParser $ \\q -> pure id \<*> v' q@

--  Applicative 'ParsecT' identity:

-- @SensitiveParser $ \\q -> v' q@

--  eta: v'

-- @SensitiveParser v'@

--  beta: v

-- @v@




-- = SensitiveParser may violate /composition/:

-- @pure (.) \<*> u \<*> v \<*> w@

--  associativity: infixl 4 \<*>

-- @((pure (.) \<*> u) \<*> v) \<*> w@

--  rename: SensitiveParser u' = u

-- @((pure (.) \<*> SensitiveParser u' ) \<*> v) \<*> w@

--  SensitiveParser left-pure:

--  rename: SensitiveParser v' = v

-- @((SensitiveParser $ \\q -> pure (.) \<*> u' q) \<*> SensitiveParser v') \<*> w@

--  expand: \<*>

-- @(applySensitiveParser (SensitiveParser $ \\q -> pure (.) \<*> u' q) (SensitiveParser v')) \<*> w@

--  expand: applySensitiveParser

-- @(SensitiveParser $ \\r -> (\\q -> pure (.) \<*> u' q) (Some (v' r)) \<*> (v' r)) \<*> w@

--  beta: (\\q -> ..)

-- @(SensitiveParser $ \\r -> pure (.) \<*> u' (Some (v' r)) \<*> (v' r)) \<*> w@

--  rename: SensitiveParser w' = w

--  expand: \<*>

--  expand: applySensitiveParser

-- @(SensitiveParser $ \\s -> (\\r -> pure (.) \<*> u' (Some (v' r)) \<*> (v' r)) (Some (w' s)) \<*> (w' s)@

--  beta: (\\r -> ..)

-- @(SensitiveParser $ \\s -> (pure (.) \<*> u' (Some (v' (Some (w' s)))) \<*> v' (Some (w' s)) \<*> (w' s)@

--  associativity: infixl 4 \<*>

-- @(SensitiveParser $ \\s -> pure (.) \<*> u' (Some (v' (Some (w' s)))) \<*> v' (Some (w' s)) \<*> (w' s)@

--  Applicative 'ParsecT' composition:

-- @(SensitiveParser $ \\s  ->  u' (Some (v' (Some (w' s)))  \<*>  ((v' (Some (w' s))) \<*> (w' s))@

--  beta: let ws = ..

-- @(SensitiveParser $ \\s  ->  let ws = w' s in  u' (Some (v' (Some ws))  \<*>  ((v' (Some ws)) \<*> ws)@

--  beta: let vt = ..

-- @(SensitiveParser $ \\s  ->  let ws = w' s in  let vt = v' (Some ws) in  u' (Some vt)  \<*>  (vt \<*> ws)@

-- ?

-- @SensitiveParser $ \\s -> u' (Some ((\\t -> v' (Some (w' t)) \<*> (w' t)) s)) \<*> ((\\t -> v' (Some (w' t)) \<*> (w' t)) s)@

--  contract: applySensitiveParser

-- @SensitiveParser u' \<*> SensitiveParser (\\t -> v' (Some (w' t)) \<*> (w' t))@

--  rename:

-- @u \<*> (SensitiveParser $ \\t -> v' (Some (w' t)) \<*> (w' t))@

--  contract: applySensitiveParser

-- @u \<*> (SensitiveParser v' \<*> SensitiveParser w')@

--  rename:

-- @u \<*> (v \<*> w)@




-- = SensitiveParser may satisfy /homomorphism/:

-- @pure f \<*> pure x@

--  ?

-- @pure (f x)@



-- = SensitiveParser may satisfy /interchange/:

-- @u \<*> pure y@

--  ?

-- @pure ($ y) \<*> u@




-- -}
-- proof_Applicative_SensitiveParser :: ()
-- proof_Applicative_SensitiveParser =  ()

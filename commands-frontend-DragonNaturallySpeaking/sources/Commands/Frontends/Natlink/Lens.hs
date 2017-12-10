{-# LANGUAGE TemplateHaskell #-}
{-| (Separate module because GHC seems to panic when [1] TemplateHaskell is
enable and [2] there is a name error (like unbound identifier)
or a type error.) -}
module Commands.Frontends.Natlink.Lens where
import Commands.Frontends.Dragon13.Extra
import Commands.Frontends.Natlink.Types

import Control.Lens(makeLenses,makePrisms,toListOf,each)

makeLenses ''ControlResults
makeLenses ''SelectionResults

makePrisms ''Utterance
makePrisms ''DragonScriptExpression
makePrisms ''PythonExpression

makePrisms ''DNSMode
makePrisms ''MicrophoneState

makeLenses ''GrammarProperties
makeLenses ''ControlGrammar
makeLenses ''SelectionGrammar
--makeLenses ''ControlConfiguration
makeLenses ''SelectionConfiguration

makePrisms ''Recognition
makePrisms ''DNSBuffer
makeLenses ''DNSToken
makeLenses ''DNSWord

-- | only the utterance ('dnsPronounced'), without metadata
rawRecognition :: Recognition -> [Text]
rawRecognition = toListOf (_Recognition.each.dnsPronounced)

-- {-NOTE when `rawRecognition` Was defined earlier in the module, i.e. before
-- the lens macros, we got this error:
--
-- [11 of 19] Compiling Commands.Frontends.Natlink.Types ( sources\Commands\Frontends\Natlink\Types.hs, .stack-work\dist\ca59d0ab\build\Commands\Frontends\Natlink\Types.o )
-- ghc.EXE: panic! (the 'impossible' happened)
--   (GHC version 8.0.2 for x86_64-unknown-mingw32):
--     initTc: unsolved constraints
--   WC {wc_insol =
--         [W] isJust_atBk :: t_atBj[tau:1] (CHoleCan: isJust)
--         [W] toListOf_atBs :: t_atBr[tau:1] (CHoleCan: toListOf)
--         [W] _Recognition_atBG :: t_atBF[tau:1] (CHoleCan: _Recognition)
--         [W] dnsPronounced_atBJ :: t_atBI[tau:1] (CHoleCan: dnsPronounced)}
--
-- Then, when we moved it later, we still got this smaller error, caused by a different function:
--
--         [11 of 19] Compiling Commands.Frontends.Natlink.Types ( sources\Commands\Frontends\Natlink\Types.hs, .stack-work\dist\ca59d0ab\build\Commands\Frontends\Natlink\Types.o )
--         ghc.EXE: panic! (the 'impossible' happened)
--           (GHC version 8.0.2 for x86_64-unknown-mingw32):
--             initTc: unsolved constraints
--           WC {wc_insol = [W] isJust_atBe :: t_atBd[tau:1] (CHoleCan: isJust)}
--
-- because we didn't import isJust.
--
-- -}

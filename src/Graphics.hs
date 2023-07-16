{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Graphics (chip8Application) where

import Data.Map (findWithDefault)
import Data.Map qualified as Map
import Interpreter (interpretN)
import Rendering (Application (..), pattern KeyPressed, pattern KeyReleased)
import SDL
    ( Event (..)
    , EventPayload (..)
    , pattern Keycode0
    , pattern Keycode1
    , pattern Keycode2
    , pattern Keycode3
    , pattern Keycode4
    , pattern Keycode5
    , pattern Keycode6
    , pattern Keycode7
    , pattern Keycode8
    , pattern Keycode9
    , pattern KeycodeA
    , pattern KeycodeB
    , pattern KeycodeC
    , pattern KeycodeD
    , pattern KeycodeE
    , pattern KeycodeF
    , pattern KeycodeN
    , pattern KeycodeP
    , pattern KeycodeQ
    , pattern KeycodeR
    , pattern KeycodeReturn
    , pattern KeycodeSpace
    )
import State (State (display, keys), renderState)
import System.Random (StdGen, split)
import Types (Coo (..), Display, KeyState (..), Nibble)

data Run = Pause | Run | Step | End
    deriving (Eq, Show)

data GameState = GameState
    { _paused :: Run
    , _state :: State
    , _count :: Int
    , _selection :: Int
    , _stdgen :: StdGen
    }

keypad :: Event -> Maybe (Nibble, KeyState)
keypad = \case
    KeyPressed n -> case n of
        Keycode1 -> Just (0x1, Pressed)
        Keycode2 -> Just (0x2, Pressed)
        Keycode3 -> Just (0x3, Pressed)
        Keycode4 -> Just (0x4, Pressed)
        Keycode5 -> Just (0x5, Pressed)
        Keycode6 -> Just (0x6, Pressed)
        Keycode7 -> Just (0x7, Pressed)
        Keycode8 -> Just (0x8, Pressed)
        Keycode9 -> Just (0x9, Pressed)
        Keycode0 -> Just (0x0, Pressed)
        KeycodeA -> Just (0xA, Pressed)
        KeycodeB -> Just (0xB, Pressed)
        KeycodeC -> Just (0xC, Pressed)
        KeycodeD -> Just (0xD, Pressed)
        KeycodeE -> Just (0xE, Pressed)
        KeycodeF -> Just (0xF, Pressed)
        _ -> Nothing
    KeyReleased n -> case n of
        Keycode1 -> Just (0x1, Released)
        Keycode2 -> Just (0x2, Released)
        Keycode3 -> Just (0x3, Released)
        Keycode4 -> Just (0x4, Released)
        Keycode5 -> Just (0x5, Released)
        Keycode6 -> Just (0x6, Released)
        Keycode7 -> Just (0x7, Released)
        Keycode8 -> Just (0x8, Released)
        Keycode9 -> Just (0x9, Released)
        Keycode0 -> Just (0x0, Released)
        KeycodeA -> Just (0xA, Released)
        KeycodeB -> Just (0xB, Released)
        KeycodeC -> Just (0xC, Released)
        KeycodeD -> Just (0xD, Released)
        KeycodeE -> Just (0xE, Released)
        KeycodeF -> Just (0xF, Released)
        _ -> Nothing
    _ -> Nothing

consumeEvent :: [(String, State)] -> GameState -> Event -> Either String GameState
consumeEvent games old@(GameState run state count _selection _stdgen) c = case c of
    KeyPressed KeycodeN ->
        let selection' = (_selection + 1) `mod` length games
            state' = snd $ games !! selection'
        in  Right $ GameState run state' 0 selection' _stdgen
    KeyPressed KeycodeP ->
        let selection' = (_selection - 1) `mod` length games
            state' = snd $ games !! selection'
        in  Right $ GameState run state' 0 selection' _stdgen
    KeyPressed KeycodeSpace ->
        let run' = case run of
                Pause -> Run
                Run -> Pause
                Step -> Pause
                x -> x
        in  Right $ GameState run' state count _selection _stdgen
    KeyPressed KeycodeReturn ->
        let run' = case run of
                Pause -> Step
                Run -> Step
                Step -> Pause
                x -> x
        in  Right $ GameState run' state count _selection _stdgen
    KeyPressed KeycodeQ -> Left "Quit"
    KeyPressed KeycodeR -> Right $ GameState run (snd $ games !! _selection) 0 _selection _stdgen
    Event _ (WindowClosedEvent _) -> Left "Quit"
    x -> case keypad x of
        Nothing -> Right old
        Just (n, Pressed) ->
            Right
                $ old{_state = state{keys = Map.insert n Pressed $ keys state}}
        Just (n, Released) ->
            Right
                $ old{_state = state{keys = Map.insert n Released $ keys state}}
renderStateLines :: GameState -> [String]
renderStateLines GameState{..} =
    (lines . renderState $ _state)
        <> [" "]
        <> [ "cycle count:" <> show _count
           , "graphic state:" <> show _paused
           ]
        <> [" "]
        <> [ "press space to pause/resume"
           , "press enter to step"
           , "press q to quit"
           , "press r to reset"
           , "press n to run the next game"
           , "press p to run the previous game"
           ]

updateState :: Int -> GameState -> (GameState, [String])
updateState _ old@(GameState Pause _state _ _ _stdgen) = (old, renderStateLines old)
updateState speed (GameState run state count state0 stdgen') =
    case interpretN stdgen'' speed' state of
        Nothing ->
            let g = GameState Pause state count state0 stdgen'''
            in  (g, renderStateLines g)
        Just state' ->
            let g = GameState run' state' (count + speed) state0 stdgen'''
            in  (g, renderStateLines g)
  where
    (stdgen'', stdgen''') = split stdgen'
    speed' = case run of
        Run -> speed
        Step -> 1
        End -> 0
    run' = case run of
        Run -> Run
        Step -> Pause
        End -> End

chip8Application :: StdGen -> Int -> [(String, State)] -> Int -> Application GameState
chip8Application stdgen speed games selection =
    Application
        { appDraw = \s -> (fst $ games !! _selection s,) . extract . display $ _state s
        , appUpdate = updateState speed
        , appHandleEvent = consumeEvent games
        , appInitialState = GameState Run (snd $ games !! selection) 0 selection stdgen
        }

extract :: Display -> [[Bool]]
extract m = do
    y <- [0 .. 31]
    pure $ do
        x <- [0 .. 63]
        pure $ findWithDefault False (Coo x y) m

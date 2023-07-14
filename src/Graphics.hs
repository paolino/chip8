{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Graphics (chip8Application) where

import Data.Map (findWithDefault)
import Interpreter (interpretN)
import Rendering (Application (..), pattern KeyPressed)
import SDL
    ( Event (..)
    , EventPayload (..)
    , pattern KeycodeN
    , pattern KeycodeP
    , pattern KeycodeQ
    , pattern KeycodeR
    , pattern KeycodeReturn
    , pattern KeycodeSpace
    )
import State (State (display), renderState)
import Types (Coo (..), Display)

data Run = Pause | Run | Step | End
    deriving (Eq, Show)

data GameState = GameState
    { _paused :: Run
    , _state :: State
    , _count :: Int
    , _selection :: Int
    }

speed :: Int
speed = 1

consumeEvent :: [(String, State)] -> GameState -> Event -> Either String GameState
consumeEvent games old@(GameState run state count _selection) c = case c of
    KeyPressed KeycodeN ->
        let selection' = (_selection + 1) `mod` length games
            state' = snd $ games !! selection'
        in  Right $ GameState run state' 0 selection'
    KeyPressed KeycodeP ->
        let selection' = (_selection - 1) `mod` length games
            state' = snd $ games !! selection'
        in  Right $ GameState run state' 0 selection'
    KeyPressed KeycodeSpace ->
        let run' = case run of
                Pause -> Run
                Run -> Pause
                Step -> Pause
                x -> x
        in  Right $ GameState run' state count _selection
    KeyPressed KeycodeReturn ->
        let run' = case run of
                Pause -> Step
                Run -> Step
                Step -> Pause
                x -> x
        in  Right $ GameState run' state count _selection
    KeyPressed KeycodeQ -> Left "Quit"
    KeyPressed KeycodeR -> Right $ GameState run (snd $ games !! _selection) 0 _selection
    Event _ (WindowClosedEvent _) -> Left "Quit"
    _ -> Right old

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

updateState :: GameState -> (GameState, [String])
updateState old@(GameState Pause _state _ _) = (old, renderStateLines old)
updateState (GameState run state count state0) =
    case interpretN speed state of
        Nothing ->
            let g = GameState Pause state count state0
            in  (g, renderStateLines g)
        Just state' ->
            let g = GameState run' state' (count + speed) state0
            in  (g, renderStateLines g)
  where
    run' = case run of
        Run -> Run
        Step -> Pause
        End -> End

chip8Application :: [(String, State)] -> Int -> Application GameState
chip8Application games selection =
    Application
        { appDraw = \s -> (fst $ games !! _selection s,) . extract . display $ _state s
        , appUpdate = updateState
        , appHandleEvent = consumeEvent games
        , appInitialState = GameState Run (snd $ games !! selection) 0 selection
        }

extract :: Display -> [[Bool]]
extract m = do
    y <- [0 .. 31]
    pure $ do
        x <- [0 .. 63]
        pure $ findWithDefault False (Coo x y) m

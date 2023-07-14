{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics (chip8Application) where

import Data.Map (findWithDefault)
import Interpreter (interpretN)
import Rendering (Application (..), pattern KeyPressed)
import SDL
    ( Event (..)
    , EventPayload (..)
    , pattern KeycodeQ
    , pattern KeycodeR
    , pattern KeycodeReturn
    , pattern KeycodeSpace
    )
import State (State (display), renderState)
import Types (Coo (..), Display)

data Run = Pause | Run | Step | End
    deriving (Eq, Show)

data GameState = GameState {_paused :: Run, _state :: State, _count :: Int}

speed :: Int
speed = 1

consumeEvent :: State -> GameState -> Event -> Either String GameState
consumeEvent state0 old@(GameState run state count) c = case c of
    KeyPressed KeycodeSpace ->
        let run' = case run of
                Pause -> Run
                Run -> Pause
                Step -> Pause
                x -> x
        in  Right $ GameState run' state count
    KeyPressed KeycodeReturn ->
        let run' = case run of
                Pause -> Step
                Run -> Step
                Step -> Pause
                x -> x
        in  Right $ GameState run' state count
    KeyPressed KeycodeQ -> Left "Quit"
    KeyPressed KeycodeR -> Right $ GameState run state0 0
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
           ]

updateState :: GameState -> (GameState, [String])
updateState old@(GameState Pause _state _) = (old, renderStateLines old)
updateState (GameState run state count) =
    case interpretN speed state of
        Nothing ->
            let g = GameState Pause state count
            in  (g, renderStateLines g)
        Just state' ->
            let g = GameState run' state' $ count + speed
            in  (g, renderStateLines g)
  where
    run' = case run of
        Run -> Run
        Step -> Pause
        End -> End

chip8Application :: State -> Application GameState
chip8Application state =
    Application
        { appDraw = extract . display . _state
        , appUpdate = updateState
        , appHandleEvent = consumeEvent state
        , appInitialState = GameState Run state 0
        }

extract :: Display -> [[Bool]]
extract m = do
    y <- [0 .. 31]
    pure $ do
        x <- [0 .. 63]
        pure $ findWithDefault False (Coo x y) m

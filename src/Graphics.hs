module Graphics (game) where

import Interpreter (interpret)
import State (State, render, renderState)
import Terminal.Game
    ( Event (KeyPress, Tick)
    , Game (Game)
    , Plane
    , blankPlane
    , bold
    , stringPlane
    , (#)
    , (%)
    , (&)
    )

data Run = Pause | Run | Step | Quit | End

data GameState = GameState {_paused :: Run, _state :: State, _count :: Int}

game :: State -> Game GameState ()
game s = Game 50 (GameState Pause s 0) f d
  where
    f _ (GameState Quit _ _) _ = Left ()
    f _ old@(GameState Pause _ _) Tick = Right old
    f _ (GameState run state count) Tick =
        case interpret state of
            Nothing -> Right $ GameState Pause state count
            Just state' -> Right $ GameState run' state' $ count + 1
      where
        run' = case run of
            Run -> Run
            Step -> Pause
            End -> End
    f _ old@(GameState paused state count) (KeyPress c) = Right $ case c of
        ' ' ->
            let
                paused' = case paused of
                    Pause -> Run
                    Run -> Pause
                    Step -> Pause
                    End -> End
             in
                GameState paused' state count
        '\n' ->
            let
                paused' = case paused of
                    Pause -> Step
                    Run -> Step
                    Step -> Pause
                    End -> End
             in
                GameState paused' state count
        'q' -> GameState Quit state count
        _ -> old
    d _genv (GameState paused state count) =
        blankPlane 64 45
            & (1, 1)
            % drawPaused paused
            # bold
            & (1, 12)
            % drawCount count
            & (38, 1)
            % drawState state
            & (4, 1)
            % drawLine 64
            & (7, 1)
            % drawDisplay state
            & (37, 1)
            % drawLine 64
            & (1, 32)
            % help

drawCount :: Int -> Plane
drawCount = stringPlane . ("at step " <>) . show

drawState :: State -> Plane
drawState = stringPlane . renderState

drawDisplay :: State -> Plane
drawDisplay = stringPlane . render

drawPaused :: Run -> Plane
drawPaused paused = stringPlane $ case paused of
    Pause -> "Paused"
    Run -> "Running"
    Step -> "Stepping"
    Quit -> "Quit"
    End -> "End"

drawLine :: Int -> Plane
drawLine n = stringPlane $ replicate n '🭹'

help :: Plane
help =
    stringPlane
        $ unlines
            [ "Press space to pause/unpause"
            , "Press enter to step"
            , "Press q to quit"
            ]
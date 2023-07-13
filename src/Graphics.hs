{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics (chip8Application) where

import Control.Monad (when)
import Data.Map (findWithDefault)
import Interpreter (interpretN)
import Rendering (Application (..), pattern KeyPressed)
import SDL
    ( Event
    , Point (..)
    , Rectangle (..)
    , Renderer
    , V2 (..)
    , V4 (..)
    , clear
    , fillRect
    , rendererDrawColor
    , ($=)
    , pattern KeycodeQ
    , pattern KeycodeR
    , pattern KeycodeReturn
    , pattern KeycodeSpace
    )
import State (State (display), renderState)
import Types (Coo (..))

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
    _ -> Right old

renderStateLines :: GameState -> [String]
renderStateLines GameState{..} =
    (lines . renderState $ _state)
        <> ["--------"]
        <> [ "cycle count:" <> show _count
           , "graphic state:" <> show _paused
           ]
        <> ["-------"]
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
        { appDraw = drawGame
        , appUpdate = updateState
        , appHandleEvent = consumeEvent state
        , appInitialState = GameState Run state 0
        , appSleep = 20000
        }

drawGame :: Renderer -> GameState -> IO ()
drawGame renderer (GameState _run state _count) = do
    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer
    rendererDrawColor renderer $= V4 0 0 255 255
    sequence_ $ do
        y <- [0 .. 31]
        x <- [0 .. 63]
        pure
            $ when
                (findWithDefault False (Coo x y) $ display state)
            $ fillRect renderer
            $ Just
            $ Rectangle
                (P $ V2 (fromIntegral x * 10) (fromIntegral y * 10))
                (V2 10 10)

{-
displayGame :: GEnv -> GameState -> Plane
displayGame _ (GameState run state count) =
    blankPlane 64 gameHeight
        & (header, 1)
            % drawPaused run
                # bold
        --
        & (header, 12)
            % drawCount count
        --
        & (header, 32)
            % help
        --
        & (headerLine, 1)
            % drawLine 64
        --
        & (window, 1)
            % drawStateDisplay state
        --
        & (footerLine, 1)
            % drawLine 64
        --
        & (footer, 1)
            % drawState state

drawCount :: Int -> Plane
drawCount = stringPlane . ("at step " <>) . show

drawState :: State -> Plane
drawState = stringPlane . renderState

drawStateDisplay :: State -> Plane
drawStateDisplay = stringPlane . render

drawPaused :: Run -> Plane
drawPaused run = stringPlane $ case run of
    Pause -> "Paused"
    Run -> "Running"
    Step -> "Stepping"
    Quit -> "Quit"
    End -> "End"
    Reset -> "Reset"

drawLine :: Int -> Plane
drawLine n = stringPlane $ replicate n '🭹'

help :: Plane
help =
    stringPlane
        $ unlines
            [ "Press space to pause/unpause"
            , "Press enter to step"
            , "Press q to quit"
            , "Press r to reset"
            ]
 -}

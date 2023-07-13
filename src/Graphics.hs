{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics where

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
    , fillRect
    , pattern KeycodeQ
    , pattern KeycodeR
    , pattern KeycodeReturn
    , pattern KeycodeSpace
    )
import State (State (display), renderState)
import Types (Coo (..))

data Run = Pause | Run | Step | Quit | End | Reset
    deriving (Eq, Show)

data GameState = GameState {_paused :: Run, _state :: State, _count :: Int}

type Row = Int
speed :: Int
speed = 1

headerHeight :: Row
headerHeight = 4

windowHeight :: Row
windowHeight = 32

header :: Row
header = 1

headerLine :: Row
headerLine = header + headerHeight

window :: Row
window = headerLine + 1

footerLine :: Row
footerLine = window + windowHeight

footer :: Row
footer = footerLine + 1

-- >>> gameHeight

gameHeight :: Row
gameHeight = headerHeight + windowHeight + footerHeight + 2
  where
    footerHeight = 8

consumeEvent :: GameState -> Event -> Either a GameState
consumeEvent old@(GameState run state count) c = Right $ case c of
    KeyPressed KeycodeSpace ->
        let
            run' = case run of
                Pause -> Run
                Run -> Pause
                Step -> Pause
                x -> x
        in
            GameState run' state count
    KeyPressed KeycodeReturn ->
        let
            run' = case run of
                Pause -> Step
                Run -> Step
                Step -> Pause
                x -> x
        in
            GameState run' state count
    KeyPressed KeycodeQ -> GameState Quit state count
    KeyPressed KeycodeR -> GameState Reset state count
    _ -> old

renderStateLines :: GameState -> [String]
renderStateLines GameState{..} = 
    (lines . renderState $ _state) ++ [show _count, show _paused]

updateState :: State -> GameState -> (GameState, [String])
updateState _ (GameState Quit state count) = let 
    g = GameState Quit state count in (g, renderStateLines g)
updateState state0 (GameState Reset state count) = let 
    g = GameState Pause state0 0 in (g, renderStateLines g)
updateState _ old@(GameState Pause state _) = (old, renderStateLines old)
updateState _ (GameState run state count) =
    case interpretN speed state of
        Nothing -> let 
            g = GameState Pause  state count in (g, renderStateLines g)
        Just state' -> let 
            g = GameState run' state' $ count + speed in (g, renderStateLines g)
  where
    run' = case run of
        Run -> Run
        Step -> Pause
        End -> End

chip8Application :: State -> Application GameState
chip8Application state =
    Application
        { appDraw = drawGame
        , appUpdate = updateState state
        , appHandleEvent = consumeEvent
        , appInitialState = GameState Run state 0
        , appSleep = 20000
        }

drawGame :: Renderer -> GameState -> IO ()
drawGame renderer (GameState _run state _count) = sequence_ $ do
    y <- [0 .. 31]
    x <- [0 .. 63]
    pure
        $ when
            (findWithDefault False (Coo x y) $ display state)
            ( fillRect
                renderer
                $ Just
                $ Rectangle 
                    (P $ V2 (fromIntegral x * 10) (fromIntegral y * 10)) 
                    (V2 10 10)
            )

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
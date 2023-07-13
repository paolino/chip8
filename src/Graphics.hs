{-# LANGUAGE PatternSynonyms #-}

module Graphics (game) where

import Data.Char (isDigit, isHexDigit, ord, toLower)
import Data.Map qualified as Map
import Interpreter (interpretN)
import State (State (..), decreaseTimers, releaseKeys, render, renderState, tickClock)
import Terminal.Game
    ( Event (KeyPress, Tick)
    , GEnv
    , Game (Game)
    , Plane
    , Row
    , blankPlane
    , bold
    , stringPlane
    , (#)
    , (%)
    , (&)
    )
import Types (Nibble)

data Run = Pause | Run | Step | Quit | End | Reset

data GameState = GameState {_paused :: Run, _state :: State, _count :: Int}

pattern KeyPause :: Event
pattern KeyPause = KeyPress ' '
pattern KeyStep :: Event
pattern KeyStep = KeyPress '\n'
pattern KeyQuit :: Event
pattern KeyQuit = KeyPress 'q'
pattern KeyReset :: Event
pattern KeyReset = KeyPress 'r'
pattern KeyKeypad :: Char -> Event
pattern KeyKeypad c <- KeyPress c
    where
        KeyKeypad c = KeyPress c

speed :: Int
speed = 1

-- Empirical proportion between speed and the amount of time a key should stay
-- pressed so that the code can detect it
keyTimeout :: Integer
keyTimeout = 20 + (40 `div` fromIntegral speed)

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

hexDigit :: Char -> Nibble
hexDigit c
    | isDigit c = fromIntegral $ ord c - ord '0'
    | isHexDigit c = fromIntegral $ ord (toLower c) - ord 'a' + 10
    | otherwise = error $ "Char `" <> [c] <> "` is not an hex digit"

game :: State -> Game GameState ()
game s = Game 50 start step displayGame
  where
    start = GameState Pause s 0
    step _ (GameState Quit _ _) _ = Left ()
    step _ (GameState Reset _ _) _ = Right start
    step _ old@(GameState Pause _ _) Tick = Right old
    step _ (GameState run state count) Tick =
        case interpretN speed state of
            Nothing -> Right $ GameState Pause state count
            Just state' -> Right $ GameState run' (tick state') $ count + speed
      where
        tick = releaseKeys keyTimeout . decreaseTimers . tickClock
        run' = case run of
            Run -> Run
            Step -> Pause
            End -> End
    step _ old@(GameState run state count) c = Right $ case c of
        KeyPause ->
            let
                run' = case run of
                    Pause -> Run
                    Run -> Pause
                    Step -> Pause
                    End -> End
            in
                GameState run' state count
        KeyStep ->
            let
                run' = case run of
                    Pause -> Step
                    Run -> Step
                    Step -> Pause
                    End -> End
            in
                GameState run' state count
        KeyQuit -> GameState Quit state count
        KeyReset -> GameState Reset state count
        KeyKeypad n
            | isHexDigit n ->
                GameState run state{keys = Map.insert (hexDigit n) (clock state) (keys state)} count
        _ -> old

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

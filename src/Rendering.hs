{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Rendering
    ( run
    , Application (..)
    , TestApplicationState
    , pattern KeyPressed
    , pattern KeyReleased
    , testApplication
    )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
    ( atomically
    , newEmptyTMVarIO
    , newTChanIO
    , putTMVar
    , readTChan
    , takeTMVar
    , writeTChan
    )
import Control.Monad.Cont
    ( ContT (ContT, runContT)
    , MonadTrans (lift)
    , forM_
    , forever
    , void
    , when
    )
import Control.Monad.Fix (fix)
import Data.Foldable (foldl', traverse_)
import SDL
    ( Event (..)
    , EventPayload (KeyboardEvent)
    , InputMotion (..)
    , KeyboardEventData (..)
    , Keycode
    , Keysym (..)
    , Point (..)
    , Renderer
    , RendererConfig (..)
    , RendererType (..)
    , V2 (..)
    , V4 (V4)
    , WindowConfig (..)
    , WindowGraphicsContext (..)
    , WindowMode (..)
    , WindowPosition (..)
    , createRenderer
    , createWindow
    , destroyWindow
    , drawLine
    , initializeAll
    , pollEvents
    , present
    , rendererDrawColor
    , ($=)
    , pattern KeycodeQ
    )
import System.Console.ANSI (clearLine, cursorUp)
import System.Random (randomRIO)

logger :: IO (IO (), ([String], Bool) -> IO ())
logger = do
    console <- newTChanIO
    let go n = do
            (ls, keepup) <- atomically $ readTChan console
            clearLine
            forM_ [0 .. n] $ \_ -> do
                cursorUp 1
                clearLine
            traverse_ putStrLn ls
            when keepup $ go (length ls)
    pure (go 0, atomically . writeTChan console)

timer :: IO (IO (), IO ())
timer = do
    block <- newEmptyTMVarIO
    let a = forever $ do
            threadDelay $ 1000000 `div` 60
            atomically $ putTMVar block ()
    pure (a, atomically $ takeTMVar block)

wc :: WindowConfig
wc =
    WindowConfig
        { windowBorder = True
        , windowHighDPI = False
        , windowInputGrabbed = False
        , windowMode = Windowed
        , windowGraphicsContext = NoGraphicsContext
        , windowPosition = Wherever
        , windowResizable = False
        , windowInitialSize = V2 640 320
        , windowVisible = True
        }

wr :: RendererConfig
wr =
    RendererConfig
        { rendererType = AcceleratedRenderer
        , rendererTargetTexture = False
        }

withThreads :: [IO ()] -> IO () -> IO ()
withThreads threads action =
    flip runContT pure $ do
        traverse_ (void . ContT . withAsync) threads
        lift action

run :: Application s -> IO ()
run application = do
    (runConsole, console) <- logger
    (runTimer, wait) <- timer
    withThreads [runConsole, runTimer] $ do
        initializeAll
        window <- createWindow "Chip8 interpreter" wc
        renderer <- createRenderer window (-1) wr
        loop wait console renderer application
        destroyWindow window

data Application s = Application
    { appDraw :: Renderer -> s -> IO ()
    , appHandleEvent :: s -> Event -> Either String s
    , appUpdate :: s -> (s, [String])
    , appInitialState :: s
    , appSleep :: Int
    }

data TestApplicationState = State
    { stateKey :: Maybe Keycode
    , stateUnit :: ()
    }
    deriving (Show)

pattern KeyPressed :: Keycode -> Event
pattern KeyPressed x <-
    Event
        _
        ( KeyboardEvent
                KeyboardEventData
                    { keyboardEventKeyMotion = Pressed
                    , keyboardEventKeysym =
                        Keysym
                            { keysymKeycode = x
                            }
                    }
            )

pattern KeyReleased :: Keycode -> Event
pattern KeyReleased x <-
    Event
        _
        ( KeyboardEvent
                KeyboardEventData
                    { keyboardEventKeyMotion = Released
                    , keyboardEventKeysym =
                        Keysym
                            { keysymKeycode = x
                            }
                    }
            )

testApplication :: Application TestApplicationState
testApplication =
    Application
        { appDraw = \renderer _ -> do
            x <- randomRIO (0, 640)
            y <- randomRIO (0, 320)
            rendererDrawColor renderer $= V4 0 0 255 255
            drawLine renderer (P (V2 0 0)) (P (V2 x y))
        , appUpdate = \s -> (s, [show s])
        , appHandleEvent =
            \s -> \case
                KeyPressed KeycodeQ -> Left "Bye!"
                KeyPressed x -> Right s{stateKey = Just x}
                KeyReleased _x -> Right s{stateKey = Nothing}
                _ -> Right s
        , appInitialState = State Nothing ()
        , appSleep = 20000
        }

loop
    :: IO ()
    -> (([String], Bool) -> IO ())
    -> Renderer
    -> Application s
    -> IO ()
loop wait console renderer Application{..} =
    ($ appInitialState) $ fix $ \go s -> do
        events <- pollEvents
        let f ms e = ms >>= \s'' -> appHandleEvent s'' e
        case foldl' f (Right s) events of
            Left l -> console ([l], False)
            Right s' -> do
                let (s'', ls) = appUpdate s'
                console (ls, True)
                appDraw renderer s''
                present renderer
                wait
                go s''

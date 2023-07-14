{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Rendering
    ( run
    , Application (..)
    , pattern KeyPressed
    , pattern KeyReleased
    )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
    ( atomically
    , newEmptyTMVarIO
    , putTMVar
    , takeTMVar
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
import Data.Text qualified as T
import Foreign.C (CInt)
import SDL
    ( Event (..)
    , EventPayload (KeyboardEvent)
    , InputMotion (..)
    , KeyboardEventData (..)
    , Keycode
    , Keysym (..)
    , Point (..)
    , Rectangle (..)
    , Renderer
    , Surface
    , V2 (..)
    , V4 (V4)
    , Window
    , WindowConfig (..)
    , WindowGraphicsContext (..)
    , WindowMode (..)
    , WindowPosition (..)
    , clear
    , createSoftwareRenderer
    , createWindow
    , destroyWindow
    , fillRect
    , getWindowSurface
    , initializeAll
    , pollEvents
    , rendererDrawColor
    , surfaceBlit
    , updateWindowSurface
    , ($=)
    )
import SDL.Font (Color, Font, initialize, load, shaded)

timer :: IO (IO (), IO ())
timer = do
    block <- newEmptyTMVarIO
    let a = forever $ do
            threadDelay $ 1000000 `div` 60
            atomically $ putTMVar block ()
    pure (a, atomically $ takeTMVar block)

wc :: CInt -> CInt -> WindowConfig
wc width height =
    WindowConfig
        { windowBorder = True
        , windowHighDPI = False
        , windowInputGrabbed = False
        , windowMode = Windowed
        , windowGraphicsContext = NoGraphicsContext
        , windowPosition = Wherever
        , windowResizable = False
        , windowInitialSize = V2 width height
        , windowVisible = True
        }

withThreads :: [IO ()] -> IO () -> IO ()
withThreads threads action =
    flip runContT pure $ do
        traverse_ (void . ContT . withAsync) threads
        lift action

run :: Application s -> IO ()
run application = do
    (runTimer, wait) <- timer
    withThreads [runTimer] $ do
        initializeAll
        initialize
        window <- createWindow "Chip8 interpreter" $ wc 640 800
        font <- load "fonts/ShareTechMono-Regular.ttf" 24
        loop wait window font application
        destroyWindow window

black :: Color
black = V4 0 0 0 255

blue :: Color
blue = V4 128 128 255 255

darkBlue :: Color
darkBlue = V4 64 64 128 255

darkGray :: Color
darkGray = V4 64 64 64 255

-- updateWindowSurface interaction

data Application s = Application
    { appDraw :: s -> [[Bool]]
    , appHandleEvent :: s -> Event -> Either String s
    , appUpdate :: s -> (s, [String])
    , appInitialState :: s
    }
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

drawConsole :: Surface -> Font -> [String] -> IO ()
drawConsole surface font ls = forM_ (zip [0 ..] ls) $ \(j, l) -> do
    txt <- shaded font darkBlue black $ T.pack l
    void $ surfaceBlit txt Nothing surface $ Just $ P $ V2 0 (344 + 24 * j)

clearWindow :: Renderer -> IO ()
clearWindow renderer = do
    rendererDrawColor renderer $= black
    clear renderer

drawGame :: Renderer -> [[Bool]] -> IO ()
drawGame renderer board = do
    rendererDrawColor renderer $= blue
    sequence_ $ do
        y <- [0 .. 31]
        x <- [0 .. 63]
        pure
            $ when
                (board !! y !! x)
            $ fillRect renderer
            $ Just
            $ Rectangle
                (P $ V2 (fromIntegral x * 10) (fromIntegral y * 10))
                (V2 10 10)

drawGrid :: Renderer -> IO ()
drawGrid renderer = do
    rendererDrawColor renderer $= darkGray
    forM_ [0 .. 16] $ \x -> do
        fillRect renderer $ Just $ Rectangle (P $ V2 (x * 80) 0) (V2 1 320)
    forM_ [0 .. 4] $ \y -> do
        fillRect renderer $ Just $ Rectangle (P $ V2 0 (y * 80)) (V2 640 1)

loop
    :: IO ()
    -> Window
    -> Font
    -> Application s
    -> IO ()
loop wait window font Application{..} = do
    surface <- getWindowSurface window
    renderer <- createSoftwareRenderer surface
    ($ appInitialState) $ fix $ \go s -> do
        events <- pollEvents
        let f ms e = ms >>= \s'' -> appHandleEvent s'' e
        case foldl' f (Right s) events of
            Left l -> putStrLn l
            Right s' -> do
                let (s'', ls) = appUpdate s'
                clearWindow renderer
                drawGrid renderer
                drawGame renderer $ appDraw s''
                drawConsole surface font ls
                updateWindowSurface window
                wait
                go s''

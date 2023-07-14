{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Rendering
    ( run
    , Application (..)
    , pattern KeyPressed
    , pattern KeyReleased
    , GraphicsParams (..)
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
import SDL.Font (Color, Font, initialize, load, shaded, size)

timer :: IO (IO (), IO ())
timer = do
    block <- newEmptyTMVarIO
    let a = forever $ do
            threadDelay $ 1000000 `div` 60
            atomically $ putTMVar block ()
    pure (a, atomically $ takeTMVar block)

withThreads :: [IO ()] -> IO () -> IO ()
withThreads threads action =
    flip runContT pure $ do
        traverse_ (void . ContT . withAsync) threads
        lift action

nameH :: GraphicsParams -> CInt
nameH GraphicsParams{gpBigFontSize} = gpBigFontSize * 10 `div` 8

boardY :: GraphicsParams -> CInt -> CInt
boardY gp = (nameH gp +)

gameH :: GraphicsParams -> CInt
gameH GraphicsParams{gpPixelSize} = 32 * gpPixelSize

consoleHH :: GraphicsParams -> CInt
consoleHH GraphicsParams{gpSmallFontSize} = 2 * gpSmallFontSize

gameY :: GraphicsParams -> CInt -> CInt
gameY gp = (consoleHH gp +) . (gameH gp +) . boardY gp

reportH :: GraphicsParams -> CInt
reportH GraphicsParams{gpSmallFontSize} = 20 * gpSmallFontSize

windowW :: GraphicsParams -> CInt
windowW GraphicsParams{gpPixelSize} = 64 * gpPixelSize + 1

windowH :: GraphicsParams -> CInt
windowH gp = gameY gp $ reportH gp

center :: CInt -> CInt -> CInt
center w w' = (w - w') `div` 2

data GraphicsParams = GraphicsParams
    { gpPixelSize :: CInt
    , gpSmallFontSize :: CInt
    , gpBigFontSize :: CInt
    }

wc :: GraphicsParams -> WindowConfig
wc gp =
    WindowConfig
        { windowBorder = True
        , windowHighDPI = False
        , windowInputGrabbed = False
        , windowMode = Windowed
        , windowGraphicsContext = NoGraphicsContext
        , windowPosition = Wherever
        , windowResizable = False
        , windowInitialSize = V2 (windowW gp) (windowH gp)
        , windowVisible = True
        }

run :: GraphicsParams -> Application s -> IO ()
run gp@GraphicsParams{..} application = do
    (runTimer, wait) <- timer
    initializeAll
    initialize
    window <- createWindow "Chip8 interpreter" $ wc gp
    fontSmall <- load "fonts/ShareTechMono-Regular.ttf" $ fromIntegral gpSmallFontSize
    fontBig <- load "fonts/ShareTechMono-Regular.ttf" $ fromIntegral gpBigFontSize
    withThreads [runTimer] $ do
        loop gp wait window fontSmall fontBig application
    destroyWindow window

data Application s = Application
    { appDraw :: s -> (String, [[Bool]])
    -- ^ (name, board)
    , appHandleEvent :: s -> Event -> Either String s
    -- ^ Left is end of application
    , appUpdate :: s -> (s, [String])
    -- ^ (new state, new state info)
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

black :: Color
black = V4 0 0 0 255

blue :: Color
blue = V4 128 128 255 255

darkBlue :: Color
darkBlue = V4 64 64 128 255

darkGray :: Color
darkGray = V4 64 64 64 255

data Rendering = Rendering
    { rcSurface :: Surface
    , rcSmallFont :: Font
    , rcBigFont :: Font
    , rcRenderer :: Renderer
    }

drawConsole :: GraphicsParams -> Rendering -> [String] -> IO ()
drawConsole gp@GraphicsParams{..} Rendering{..} ls =
    forM_ (zip [0 ..] ls) $ \(j, l) -> do
        txt <- shaded rcSmallFont darkBlue black $ T.pack l
        void
            $ surfaceBlit txt Nothing rcSurface
            $ Just
            $ P
            $ V2 0 (gameY gp $ gpSmallFontSize * j)

clearWindow :: Rendering -> IO ()
clearWindow Rendering{rcRenderer} = do
    rendererDrawColor rcRenderer $= black
    clear rcRenderer

drawGame :: GraphicsParams -> Rendering -> (String, [[Bool]]) -> IO ()
drawGame gp@GraphicsParams{..} Rendering{..} (name, board) = do
    rendererDrawColor rcRenderer $= blue
    txt <- shaded rcBigFont darkBlue black $ T.pack name
    (sz, _) <- size rcBigFont $ T.pack name
    void
        $ surfaceBlit txt Nothing rcSurface
        $ Just
        $ P
        $ V2 (center (windowW gp) $ fromIntegral sz) 0
    sequence_ $ do
        y <- [0 .. 31]
        x <- [0 .. 63]
        pure
            $ when (board !! y !! x)
            $ fillRect rcRenderer
            $ Just
            $ Rectangle
                ( P
                    $ V2
                        (fromIntegral x * gpPixelSize)
                        (boardY gp $ fromIntegral y * gpPixelSize)
                )
                (V2 gpPixelSize gpPixelSize)

drawGrid :: GraphicsParams -> Rendering -> IO ()
drawGrid gp@GraphicsParams{..} Rendering{..} = do
    rendererDrawColor rcRenderer $= darkGray
    forM_ [0 .. 16] $ \x -> do
        fillRect rcRenderer
            $ Just
            $ Rectangle (P $ V2 (x * 8 * gpPixelSize) (boardY gp 0)) (V2 1 $ 32 * gpPixelSize)
    forM_ [0 .. 4] $ \y -> do
        fillRect rcRenderer
            $ Just
            $ Rectangle (P $ V2 0 (boardY gp $ y * 8 * gpPixelSize)) (V2 (64 * gpPixelSize) 1)

loop
    :: GraphicsParams
    -> IO ()
    -> Window
    -> Font
    -> Font
    -> Application s
    -> IO ()
loop gp wait window fontSmall fontBig Application{..} = do
    surface <- getWindowSurface window
    renderer <- createSoftwareRenderer surface
    let rendering = Rendering surface fontSmall fontBig renderer
    ($ appInitialState) $ fix $ \go s -> do
        events <- pollEvents
        let f ms e = ms >>= \s'' -> appHandleEvent s'' e
        case foldl' f (Right s) events of
            Left l -> putStrLn l
            Right s' -> do
                let (s'', ls) = appUpdate s'
                clearWindow rendering
                drawGrid gp rendering
                drawGame gp rendering $ appDraw s''
                drawConsole gp rendering ls
                updateWindowSurface window
                wait
                go s''

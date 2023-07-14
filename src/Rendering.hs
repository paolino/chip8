{-# LANGUAGE NamedFieldPuns #-}
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

pixelSize :: CInt
pixelSize = 20

smallFontSize :: CInt
smallFontSize = 24

bigFontSize :: CInt
bigFontSize = 32

nameH :: CInt
nameH = bigFontSize * 10 `div` 8

boardY :: CInt -> CInt
boardY = (nameH +)

gameH :: CInt
gameH = 32 * pixelSize

consoleHH :: CInt
consoleHH = 2 * smallFontSize

gameY :: CInt -> CInt
gameY = (consoleHH +) . (gameH +) . boardY

reportH :: CInt
reportH = 20 * smallFontSize

windowW :: CInt
windowW = 64 * pixelSize + 1

windowH :: CInt
windowH = gameY reportH

center :: CInt -> CInt -> CInt
center w w' = (w - w') `div` 2

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
        , windowInitialSize = V2 windowW windowH
        , windowVisible = True
        }

run :: Application s -> IO ()
run application = do
    (runTimer, wait) <- timer
    initializeAll
    initialize
    window <- createWindow "Chip8 interpreter" wc
    fontSmall <- load "fonts/ShareTechMono-Regular.ttf" $ fromIntegral smallFontSize
    fontBig <- load "fonts/ShareTechMono-Regular.ttf" $ fromIntegral bigFontSize
    withThreads [runTimer] $ do
        loop wait window fontSmall fontBig application
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

drawConsole :: Rendering -> [String] -> IO ()
drawConsole Rendering{..} ls = forM_ (zip [0 ..] ls) $ \(j, l) -> do
    txt <- shaded rcSmallFont darkBlue black $ T.pack l
    void $ surfaceBlit txt Nothing rcSurface $ Just $ P $ V2 0 (gameY $ smallFontSize * j)

clearWindow :: Rendering -> IO ()
clearWindow Rendering{rcRenderer} = do
    rendererDrawColor rcRenderer $= black
    clear rcRenderer

drawGame :: Rendering -> (String, [[Bool]]) -> IO ()
drawGame Rendering{..} (name, board) = do
    rendererDrawColor rcRenderer $= blue
    txt <- shaded rcBigFont darkBlue black $ T.pack name
    (sz, _) <- size rcBigFont $ T.pack name
    void $ surfaceBlit txt Nothing rcSurface $ Just $ P $ V2 (center windowW $ fromIntegral sz) 0
    sequence_ $ do
        y <- [0 .. 31]
        x <- [0 .. 63]
        pure
            $ when (board !! y !! x)
            $ fillRect rcRenderer
            $ Just
            $ Rectangle
                (P $ V2 (fromIntegral x * pixelSize) (boardY $ fromIntegral y * pixelSize))
                (V2 pixelSize pixelSize)

drawGrid :: Rendering -> IO ()
drawGrid Rendering{..} = do
    rendererDrawColor rcRenderer $= darkGray
    forM_ [0 .. 16] $ \x -> do
        fillRect rcRenderer $ Just $ Rectangle (P $ V2 (x * 8 * pixelSize) (boardY 0)) (V2 1 $ 32 * pixelSize)
    forM_ [0 .. 4] $ \y -> do
        fillRect rcRenderer $ Just $ Rectangle (P $ V2 0 (boardY $ y * 8 * pixelSize)) (V2 (64 * pixelSize) 1)

loop
    :: IO ()
    -> Window
    -> Font
    -> Font
    -> Application s
    -> IO ()
loop wait window fontSmall fontBig Application{..} = do
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
                drawGrid rendering
                drawGame rendering $ appDraw s''
                drawConsole rendering ls
                updateWindowSurface window
                wait
                go s''

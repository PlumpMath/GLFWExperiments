module Main (main) where

--------------------------------------------------------------------------------
-- INCLUDES

import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad             (unless, when, void)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, liftIO, modify, put)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List                 (intercalate)
import Data.Maybe                (catMaybes)
import Data.Foldable			 (forM_)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

-- Game includes
import Utility
import Hallway
import TileMap
import Tile

--------------------------------------------------------------------------------
-- Data types

--Env, used to store environment variables
data Env = Env
	{ 
		envEventsChannel    :: TQueue Event,
		envWindow           :: !GLFW.Window,
		envZDistClosest     :: !Double,
		envZDistFarthest    :: !Double
	}

--State, used to store variables that change with the state of the game
data State = State
	{ 
		stateWindowWidth     :: !Int,
		stateWindowHeight    :: !Int,
		stateXAngle          :: !Double,
		stateYAngle          :: !Double,
		stateZAngle          :: !Double,
		stateZDist           :: !Double,
		stateMouseDown       :: !Bool
	}

type Game = RWST Env () State IO

data Event =  EventError           !GLFW.Error !String
			| EventWindowPos       !GLFW.Window !Int !Int
			| EventWindowSize      !GLFW.Window !Int !Int
			| EventWindowClose     !GLFW.Window
			| EventWindowRefresh   !GLFW.Window
			| EventWindowFocus     !GLFW.Window !GLFW.FocusState
			| EventWindowIconify   !GLFW.Window !GLFW.IconifyState
			| EventFramebufferSize !GLFW.Window !Int !Int
			| EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
			| EventCursorPos       !GLFW.Window !Double !Double
			| EventCursorEnter     !GLFW.Window !GLFW.CursorState
			| EventScroll          !GLFW.Window !Double !Double
			| EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
			| EventChar            !GLFW.Window !Char
			deriving Show


--------------------------------------------------------------------------------
-- Main, startup code
main :: IO ()
main = do
	--(w,h) <- liftIO $ GLFW.getMonitorPhysicalSize mon
	let 
		width  = 1200
		height = 800
		tileMap = genLevel
	eventsChannel <- newTQueueIO :: IO (TQueue Event)
	withWindow width height "Neurocaster" $ \win -> do 
		--Initialize GLFW callbacks
		GLFW.setErrorCallback				$ Just $ errorCallback           eventsChannel
		GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       eventsChannel
		GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      eventsChannel
		GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     eventsChannel
		GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   eventsChannel
		GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     eventsChannel
		GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   eventsChannel
		GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventsChannel
		GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     eventsChannel
		GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       eventsChannel
		GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     eventsChannel
		GLFW.setScrollCallback          win $ Just $ scrollCallback          eventsChannel
		GLFW.setKeyCallback             win $ Just $ keyCallback             eventsChannel
		GLFW.setCharCallback            win $ Just $ charCallback            eventsChannel

		GLFW.swapInterval 1

		--OpenGL initialization
		--GL.position (GL.Light 0) GL.$= GL.Vertex4 5 5 10 0
		GL.light    (GL.Light 0) GL.$= GL.Enabled
		GL.lighting   GL.$= GL.Enabled
		--GL.cullFace   GL.$= Just GL.Back
		GL.depthFunc  GL.$= Just GL.Less
		GL.clearColor GL.$= GL.Color4 0 0 0 1
		GL.normalize  GL.$= GL.Enabled

		let 
			zDistClosest  = 1
			zDistFarthest = zDistClosest + 4
			zDist         = zDistClosest + ((zDistFarthest - zDistClosest) / 2)
			env = Env
				{ 
				envEventsChannel    = eventsChannel,
				envWindow        = win,
				envZDistClosest  = zDistClosest,
				envZDistFarthest = zDistFarthest
				}

			state = State
				{ 
				stateWindowWidth     = width,
				stateWindowHeight    = height,
				stateXAngle          = 0,
				stateYAngle          = 0,
				stateZAngle          = 0,
				stateZDist           = zDist,
				stateMouseDown       = False
				}
		runGame env state
	putStrLn "DONE"

--------------------------------------------------------------------------------
-- Game Loop

-- Start the game loop
runGame :: Env -> State -> IO ()
runGame env state = do
	void $ evalRWST (adjustWindow >> run) env state
	--void $ evalRWST (run) env state

-- The main game loop
run :: Game ()
run = do
	win <- asks envWindow

	draw
	liftIO $ do
		GLFW.swapBuffers win
		GL.flush  -- not necessary, but someone recommended it
		GLFW.pollEvents
	processEvents

	q <- liftIO $ GLFW.windowShouldClose win
	unless q run

draw :: Game ()
draw = do
	env   <- ask
	state <- get
	liftIO $ do
		GL.clear [GL.ColorBuffer, GL.DepthBuffer]
		render $ getRenderInfo $ BasicHallway 0 0

--------------------------------------------------------------------------------
-- GLFW Event Handling

processEvents :: Game ()
processEvents = do
	tc <- asks envEventsChannel
	me <- liftIO $ atomically $ tryReadTQueue tc
	case me of
		Just e -> do
			processEvent e
			processEvents
		Nothing -> return ()

processEvent :: Event -> Game ()
processEvent ev =
	case ev of
		(EventError e s) -> do
			--printEvent "error" [show e, show s]
			win <- asks envWindow
			liftIO $ GLFW.setWindowShouldClose win True

		--(EventWindowPos _ x y) ->
			--printEvent "window pos" [show x, show y]

		(EventWindowSize _ width height) -> do
			--printEvent "window size" [show width, show height]
			modify $ \s -> s
				{ stateWindowWidth  = width
				, stateWindowHeight = height
				}
			--adjustWindow

		--(EventWindowClose _) ->
			--printEvent "window close" []

		--(EventWindowRefresh _) ->
			--printEvent "window refresh" []

		--(EventWindowFocus _ fs) ->
			--printEvent "window focus" [show fs]

		--(EventWindowIconify _ is) ->
			--printEvent "window iconify" [show is]

		--(EventFramebufferSize _ w h) ->
			--printEvent "framebuffer size" [show w, show h]

		--(EventMouseButton _ mb mbs mk) -> do
			--printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
			--when (mb == GLFW.MouseButton'1) $ do
				--let pressed = mbs == GLFW.MouseButtonState'Pressed
				--modify $ \s -> s
				--	{ stateMouseDown = pressed
				--	}
				--unless pressed $
				--	modify $ \s -> s
				--	{ stateDragging = False
				--	}

		--(EventCursorPos _ x y) -> do
			--let 
				--x' = round x :: Int
				--y' = round y :: Int
			--printEvent "cursor pos" [show x', show y']
			--state <- get
			--when (stateMouseDown state && not (stateDragging state)) $
			--	put $ state
			--		{ stateDragging        = True
			--		, stateDragStartX      = x
			--		, stateDragStartY      = y
			--		, stateDragStartXAngle = stateXAngle state
			--		, stateDragStartYAngle = stateYAngle state
			--		}

		--(EventCursorEnter _ cs) ->
			--printEvent "cursor enter" [show cs]

		--(EventScroll _ x y) -> do
			--let 
				--x' = round x :: Int
				--y' = round y :: Int
			--printEvent "scroll" [show x', show y']
			--env <- ask
			--modify $ \s -> s
			--	{ stateZDist =
			--		let zDist' = stateZDist s + realToFrac (negate $ y / 2)
			--		in clip (envZDistClosest env) (envZDistFarthest env) zDist'
			--	}
			--adjustWindow

		(EventKey win k scancode ks mk) -> do
			--printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]
			when (ks == GLFW.KeyState'Pressed) $ do
				-- Q, Esc: exit
				when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
					liftIO $ GLFW.setWindowShouldClose win True
				-- ?: print instructions
				--when (k == GLFW.Key'Slash && GLFW.modifierKeysShift mk) $
					--liftIO printInstructions
				-- i: print GLFW information
				--when (k == GLFW.Key'I) $
					--liftIO $ printInformation win

		--(EventChar _ c) ->
			--printEvent "char" [show c]
		otherwise -> return ()

adjustWindow :: Game ()
adjustWindow = do
	state <- get
	let 
		width  = stateWindowWidth  state
		height = stateWindowHeight state
		zDist  = stateZDist        state

	let 
		pos   = GL.Position 0 0
		size  = GL.Size (fromIntegral width) (fromIntegral height)
		h     = fromIntegral height / fromIntegral width :: Double
		znear = 1           :: Double
		zfar  = 40          :: Double
		xmax  = znear * 0.5 :: Double
	liftIO $ do
		GL.viewport   GL.$= (pos, size)
		GL.matrixMode GL.$= GL.Projection
		GL.loadIdentity
		GL.frustum 
			(realToFrac $ -xmax)
			(realToFrac    xmax)
			(realToFrac $ -xmax * realToFrac h)
			(realToFrac $  xmax * realToFrac h)
			(realToFrac    znear)
			(realToFrac    zfar)
		GL.matrixMode GL.$= GL.Modelview 0
		GL.loadIdentity
		GL.translate (GL.Vector3 0 0 (negate $ realToFrac zDist) :: GL.Vector3 GL.GLfloat)

--------------------------------------------------------------------------------
-- Events Callbacks

-- These arrive from GLFW
-- Each callback does just one thing: write an appropriate Event to the events TQueue.

errorCallback           :: TQueue Event -> GLFW.Error -> String                                                            -> IO ()
errorCallback           tc e s            = atomically $ writeTQueue tc $ EventError           e s
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      win w h
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowCloseCallback     tc win            = atomically $ writeTQueue tc $ EventWindowClose     win
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     :: TQueue Event -> GLFW.Window -> GLFW.FocusState                                                  -> IO ()
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> GLFW.IconifyState                                                -> IO ()
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          win x y
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()
keyCallback             tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey             win k sc ka mk
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                             -> IO ()
charCallback            tc win c          = atomically $ writeTQueue tc $ EventChar            win c



--------------------------------------------------------------------------------
-- Keyboard Input

getCursorKeyDirections :: GLFW.Window -> IO (Double, Double)
getCursorKeyDirections win = do
	x0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Up
	x1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Down
	y0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Left
	y1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Right
	let 
		x0n = if x0 then (-1) else 0
		x1n = if x1 then   1  else 0
		y0n = if y0 then (-1) else 0
		y1n = if y1 then   1  else 0
	return (x0n + x1n, y0n + y1n)

isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed   = True
isPress GLFW.KeyState'Repeating = True
isPress _                       = False


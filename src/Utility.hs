module Utility where

--------------------------------------------------------------------------------
-- INCLUDES

import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad             (unless, when, void)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, liftIO, modify, put)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List                 (intercalate)
import Data.Maybe                (catMaybes)
import qualified Data.Map as Map

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import Math

--------------------------------------------------------------------------------
-- GLFW-b is made to be very close to the C API, so creating a window is pretty
-- clunky by Haskell standards. A higher-level API would have some function
-- like withWindow.

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
	GLFW.setErrorCallback $ Just simpleErrorCallback
	r <- GLFW.init
	when r $ do
		m <- GLFW.createWindow width height title Nothing Nothing
		case m of
			(Just win) -> do
				GLFW.makeContextCurrent m
				f win
				GLFW.setErrorCallback $ Just simpleErrorCallback
				GLFW.destroyWindow win
			Nothing -> return ()
		GLFW.terminate
	where
		simpleErrorCallback e s =
			putStrLn $ unwords [show e, show s]

--------------------------------------------------------------------------------
-- Get Monitor Info
type MonitorInfo = (String, (Int,Int), (Int,Int), [GLFW.VideoMode])

getMonitorInfos :: MaybeT IO [MonitorInfo]
getMonitorInfos =
		getMonitors >>= mapM getMonitorInfo
	where
		getMonitors :: MaybeT IO [GLFW.Monitor]
		getMonitors = MaybeT GLFW.getMonitors

		getMonitorInfo :: GLFW.Monitor -> MaybeT IO MonitorInfo
		getMonitorInfo mon = do
				name <- getMonitorName mon
				vms  <- getVideoModes mon
				MaybeT $ do
						pos  <- liftIO $ GLFW.getMonitorPos mon
						size <- liftIO $ GLFW.getMonitorPhysicalSize mon
						return $ Just (name, pos, size, vms)

		getMonitorName :: GLFW.Monitor -> MaybeT IO String
		getMonitorName mon = MaybeT $ GLFW.getMonitorName mon

		getVideoModes :: GLFW.Monitor -> MaybeT IO [GLFW.VideoMode]
		getVideoModes mon = MaybeT $ GLFW.getVideoModes mon

--------------------------------------------------------------------------------
-- OpenGL helper functions
--------------------------------------------------------------------------------


vertex :: Float -> Float -> Float -> IO ()
vertex x y z =
		GL.vertex $ mkVertex x y z

mkVertex :: Float -> Float -> Float -> GL.Vertex3 GL.GLfloat
mkVertex x y z =
		GL.Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

normal :: Float -> Float -> Float -> IO ()
normal x y z =
		GL.normal $ mkNormal x y z

mkNormal :: Float -> Float -> Float -> GL.Normal3 GL.GLfloat
mkNormal x y z =
		GL.Normal3 (realToFrac x) (realToFrac y) (realToFrac z)

--------------------------------------------------------------------------------
--Container for different primitive renderables
data RenderInfo = 
		BasicRenderInfo GL.PrimitiveMode [GL.Vertex3 GL.GLfloat] (GL.Color4 GL.GLfloat)
	|	RenderInfoN GL.PrimitiveMode [GL.Vertex3 GL.GLfloat] [GL.Vertex3 GL.GLfloat] (GL.Color4 GL.GLfloat)

--------------------------------------------------------------------------------
-- |'render'
-- Renders a primitive solid object, give a RenderInfo value

render :: RenderInfo -> IO ()
render (BasicRenderInfo mode vertices color) = do
		--GL.defineNewList GL.Compile $ do
		GL.colorMaterial GL.$= Just (GL.Front, GL.AmbientAndDiffuse)
		GL.color color
		GL.blend GL.$= GL.Enabled
		GL.shadeModel GL.$= GL.Flat
		GL.renderPrimitive mode $ drawVerts vertices
	where
		drawVerts (x:xs) = GL.vertex x >> drawVerts xs
		drawVerts [] = return ()

renderMap :: (Map.Map Integer RenderInfo) -> Map.Map Integer (IO ())
renderMap m = Map.map (render) m
		

--------------------------------------------------------------------------------
-- | 'clip'
-- Clips the input to the boundaries
clip :: Ord a => a -> a -> a -> a
clip l h x
	| x < l     = l
	| x > h     = h
	| otherwise = x

--------------------------------------------------------------------------------
-- | 'oneD'
-- Converts a two dimensional position to one dimensional position
oneD :: (Integer, Integer) -> Integer -> Integer
oneD (x,y) w = y * w + x


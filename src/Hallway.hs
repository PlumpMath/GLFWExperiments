module Hallway where

--------------------------------------------------------------------------------
-- INCLUDES

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import Control.Monad (forM_, when)

import Utility
import Tile


--------------------------------------------------------------------------------
-- renderHallway
--
-- Given a hallway with specific coordinates, returns the renderInfo for that hallway
-- RenderInfo value can be passed to a rendering function, such as renderSolid
-- So that it is rendered to the GPU
getRenderInfo :: Tile -> RenderInfo
getRenderInfo (BasicHallway x y) = BasicRenderInfo GL.Quads vertices color
   	where
   		w = 0.5
   		color = GL.Color4 0.25 1.0 1.0 1.0
   		vertices = 
   			mkVertex ((fromIntegral x) -w) ((fromIntegral y)-w) 0 :
   			mkVertex ((fromIntegral x) -w) ((fromIntegral y)+w) 0 :
   			mkVertex ((fromIntegral x) +w) ((fromIntegral y)+w) 0 :
   			mkVertex ((fromIntegral x) +w) ((fromIntegral y)-w) 0 : []

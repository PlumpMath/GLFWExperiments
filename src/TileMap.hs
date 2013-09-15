-- | The 'TileMap' module contains functions pertaining to the TileMap data type.
module TileMap where

--------------------------------------------------------------------------------
-- INCLUDES

import qualified Graphics.Rendering.OpenGL 	as GL
import qualified Graphics.UI.GLFW          	as GLFW
import qualified Data.Map					as Map

import Utility
import Tile
import Hallway

-- | A 'TileMap' is a binary tree map that contains the map tiles in a level.
type TileMap = Map.Map Integer Tile

--------------------------------------------------------------------------------
-- | 'genLevel'
-- Generates a new Tile Map Level
genLevel :: Integer -> Integer -> TileMap
genLevel width height = getTileMap $ width * height
	where
		getTileMap n = Map.insert n (BasicHallway n n) $ getTileMap (n-1)
		getTileMap 0 = Map.empty

--------------------------------------------------------------------------------
-- | 'insertSubSystem'
-- inserts a Sub System into the map, including floors, ports and nodes
insertSubSystem :: (Integer,Integer) -> Integer -> TileMap -> TileMap
insertSubSystem (x,y) w t = insertNode $ insertFloor floorPositions $ insertPort1 $ insertPort2 t
	where
		floorPositions = [ (i,j) | i <- [(x-1)..(x+1)], j <- [(y-1)..(y+1)] ]
		insertFloor ((i,j):ps) t = Map.insert (oneD (i,j) w) (BasicHallway i j) $ insertFloor ps t
		insertPort1 t = Map.insert (oneD (x+2,y) w) (BasicHallway (x+2) y) t
		insertPort2 t = Map.insert (oneD (x-2,y) w) (BasicHallway (x-2) y) t
		insertNode t = Map.insert (oneD (x,y) w) (BasicHallway x y) t

--------------------------------------------------------------------------------
-- | 'mapRenderInfo'
-- Gets a Map of the render infos in a map
mapRenderInfo :: TileMap -> Map.Map Integer RenderInfo
mapRenderInfo t = Map.map (getRenderInfo) t

	

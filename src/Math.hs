-- | The 'Math' module contains functions that perform various mathematical expressions.
module Math where

--------------------------------------------------------------------------------
-- INCLUDES

import Graphics.Rendering.OpenGL
--import qualified Graphics.UI.GLFW          	as GLFW

import Data.Ix

--------------------------------------------------------------------------------
-- * Data types

-- |'NormPlan', represents a plane, with a given coordinate and normal
data NormPlane = NormPlane (Vector3 GLfloat) (Vector3 GLfloat)

-- | Make Vector3 inherit from Num so that we can use all of the nice operator overloading.
instance Num (Vector3 GLfloat) where
	(+) (Vector3 x y z) (Vector3 x2 y2 z2) = Vector3 (x + x2) (y + y2) (z + z2)
	(*) (Vector3 x y z) (Vector3 x2 y2 z2) = Vector3 (x * x2) (y * y2) (z * z2)
	(-) (Vector3 x y z) (Vector3 x2 y2 z2) = Vector3 (x - x2) (y - y2) (z - z2)
	negate (Vector3 x y z) = Vector3 (-x) (-y) (-z)
	abs (Vector3 x y z) = Vector3 (abs x) (abs y) (abs z)
	signum (Vector3 x y z) = Vector3 (signum x) (signum y) (signum z)
	
-- | Make Vector3 inherit from Eq so you can compare them
instance Eq (Vector3 GLfloat) where
	(==) (Vector3 x y z) (Vector3 x2 y2 z2) = (x == x2) && (y == y2) && (z == z2)
	(/=) (Vector3 x y z) (Vector3 x2 y2 z2) = (x /= x2) && (y /= y2) && (z /= z2)

-- | This class is for those data types that you should be able to interpolate between a start and end value, with a given mu 
class Interpolatable a where
	lerp :: a -> a -> GLfloat -> a

-- | Give GLfloat interpolatable abilities
instance Interpolatable GLfloat where
	lerp a b mu = ((-1) * mu) * a + mu * b

-- |Give Vector3 interpolatable abilities
instance Interpolatable (Vector3 GLfloat) where
	lerp (Vector3 x y z) (Vector3 x2 y2 z2) mu = Vector3 (lerp x x2 mu) (lerp y y2 mu) (lerp z z2 mu)

--------------------------------------------------------------------------------
-- * Vector Math

-- |'direction', finds the directions vector between two vectors.
direction :: Vector3 GLfloat -> Vector3 GLfloat -> Vector3 GLfloat
direction v v2 = v - v2

-- |'magnitude', finds the magnitude value of a vector.
magnitude :: Vector3 GLfloat -> GLfloat
magnitude (Vector3 x y z) = sqrt (x^2 + y^2 + z^2)

-- |'squaredMagnitude', this is cheaper than finding the actual magnitude. 
--  Instead of relying upon a square root, it performs a dot product on itself with itself.
squaredMagnitude :: Vector3 GLfloat -> GLfloat
squaredMagnitude v = dotProduct v v

-- |'reciprocal', finds the inverse of a given vector
reciprocal :: Vector3 GLfloat -> Vector3 GLfloat
reciprocal (Vector3 x y z) = Vector3 (1 / x) (1 / y) (1 / z)

-- |'normalizeV', normalizes a vector, converting it to a unit vector.
normalizeV :: Vector3 GLfloat -> Vector3 GLfloat
normalizeV v = (reciprocal v) * v

-- |'dotProduct', returns the dot product of the given vectors.
dotProduct :: Vector3 GLfloat -> Vector3 GLfloat -> GLfloat
dotProduct (Vector3 x y z) (Vector3 x2 y2 z2) = (x * x2) + (y * y2) + (z * z2)

-- |'crossProduct',returns the cross product of the given vectors.
crossProduct :: Vector3 GLfloat -> Vector3 GLfloat -> Vector3 GLfloat
crossProduct (Vector3 x y z) (Vector3 x2 y2 z2) = Vector3 crossX crossY crossZ
	where
		crossX = y * z2 - z * y2
		crossY = z * x2 - x * z2
		crossZ = x * y2 - y * x2

-- |'isColinear', determines whether or not two vectors are colinear
isColinear :: Vector3 GLfloat -> Vector3 GLfloat -> Bool
isColinear v v2 = angle == 0
	where
		dot = dotProduct v v2
		magMul = (magnitude v) * (magnitude v2)
		angle = acos ( dot / magMul )

-- |'isColinear180', determines whether or not two vectors are colinear, and facing opposite directions.
isColinear180 :: Vector3 GLfloat -> Vector3 GLfloat -> Bool
isColinear180 v v2 = angle == 180
	where
		dot = dotProduct v v2
		magMul = (magnitude v) * (magnitude v2)
		angle = acos ( dot / magMul )

-- |'isPerpendicular', determines if the the provided vectors are perpendicular.
isPerpendicular :: Vector3 GLfloat -> Vector3 GLfloat -> Bool
isPerpendicular v v2 = (dotProduct v v2) == 90

-- |'isSameDirection', determines if the given vectors face the same direction.
isSameDirection :: Vector3 GLfloat -> Vector3 GLfloat -> Bool
isSameDirection v v2 = (dotProduct v v2) > 0

-- |'isSameDirection', determines if the given vectors face the opposite direction.
isOppositeDirection :: Vector3 GLfloat -> Vector3 GLfloat -> Bool
isOppositeDirection v v2 = (dotProduct v v2) < 0

-- |'heightFromPlane', returns the relative height of the given coordinate to the given plate.
heightFromPlane :: NormPlane -> Vector3 GLfloat -> GLfloat
heightFromPlane (NormPlane planePos planeNorm) point = dotProduct (point - planePos) planeNorm

-- |'isOnPlane', determines if the given coordinate is on the given plane.
isOnPlane :: NormPlane -> Vector3 GLfloat -> Bool
isOnPlane plane point = height == 0
	where
		height = heightFromPlane plane point

-- |'isAbovePlane', determines if the given coordinate is above the given plane.
isAbovePlane :: NormPlane -> Vector3 GLfloat -> Bool
isAbovePlane plane point = height > 0
	where
		height = heightFromPlane plane point

-- |'isBelowPlane', determines if the given coordinate is below the given plane
isBelowPlane :: NormPlane -> Vector3 GLfloat -> Bool
isBelowPlane plane point = height < 0
	where
		height = heightFromPlane plane point

-- |'normal', returns the normal vector of the given surface.
normal :: [Vector3 GLfloat] -> Vector3 GLfloat
normal (a:b:c:[]) = normalizeV distance
	where
		distance = crossProduct (b-a) (c-a)

-- |'toVertex', converts the given vector to a vertex type.
toVertex :: Vector3 GLfloat -> Vertex3 GLfloat
toVertex (Vector3 x y z) = Vertex3 x y z

--------------------------------------------------------------------------------
-- * Matrix Math

--class VertexIndexable a wheremg
--	(!) :: a -> Integer -> b

data VertexComponent a => MatrixComponent3 a = MatrixComponent3 (a,a,a)
data VertexComponent a => MatrixComponent4 a = MatrixComponent4 (a,a,a)
data VertexComponent a => Matrix3x3 a = Matrix3x3 ( MatrixComponent3 a, MatrixComponent3 a, MatrixComponent3 a)
data VertexComponent a => Matrix4x4 a = Matrix4x4 ( MatrixComponent4 a, MatrixComponent4 a, MatrixComponent4 a)

--(!) :: VertexComponent a => Matrix3x3 a -> Integer -> MatrixComponent3 a

--instance VertexIndexable (MatrixComponent3 (VertexComponent a) ) where
--	(!) :: forall b. MatrixComponent3 (VertexComponent a) -> Integer -> b
--	(!) (Matrix3x3 (f,s,t)) index
--		| index == 0 = f
--		| index == 1 = s
--		| index == 2 = t

--instance VertexIndexable Matrix3x3 where
	--(!) :: VertexComponent a => Matrix3x3 a -> Integer -> a
--	(!) (Matrix3x3 (f,s,t)) index
--		| index == 0 = f
--		| index == 1 = s
--		| index == 2 = t

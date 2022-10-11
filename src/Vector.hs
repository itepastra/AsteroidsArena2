module Vector where

data Wall = Wall
  { base :: Point Double,
    normal :: Vector Double,
    wvelocity :: Velocity
  }

type Position = Point Double

type Normal = Vector Double

type Velocity = Vector Double

class PhysicsObject a where
  timeStep :: Num dt => a -> dt -> a
  accelerate :: Num dt => a -> Vector Double -> dt -> a
  collides :: PhysicsObject b => a -> b -> Bool
  inWall :: a -> Wall -> Bool

data Point a = Point a a

data Vector a = Vector a a

type Radius = Double

vMult :: Num a => Vector a -> b -> Vector a
vMult (Vector x y) m = Vector (x * m) (y * m)

vAdd :: Num a => Vector a -> Vector a -> Vector a
vAdd (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)

vAbs2 :: Num a => Vector a -> a
vAbs2 v = vDot v v

vDot :: Num a => Vector a -> Vector a -> a
vDot (Vector x1 y1) (Vector x2 y2) = x1 * x2 + y1 * y2

vAbs :: Floating a => Vector a -> a
vAbs v = sqrt (vAbs2 v)

pDist2 :: Num a => Point a -> Point a -> a
pDist2 (Point x1 y1) (Point x2 y2) = x * x + y * y
  where
    x = x2 - x1
    y = y2 - y1

vNorm :: Floating a => Vector a -> Vector a
vNorm v = v `vMult` (1 / vAbs v)

pvAdd :: Num a => Point a -> Vector a -> Point a
pvAdd (Point x y) (Vector v z) = Point (x + v) (v + z)
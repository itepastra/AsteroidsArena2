module Vector where

data Point a = Point a a

data Vector a = Vector a a

vMult :: Num a => Vector a -> a -> Vector a
vMult (Vector x y) m = Vector (x * m) (y * m)

vAbs2 :: Num a => Vector a -> a
vAbs2 v = vDot v v

vDot :: Num a => Vector a -> Vector a -> a
vDot (Vector x1 y1) (Vector x2 y2) = x1 * x2 + y1 * y2

vAbs :: Floating a => Vector a -> a
vAbs v = sqrt (vAbs2 v)

pDist2 :: Num a => Point a -> Point a -> a
pDist2 (Point x1 y1) (Point x2 y2) = x * x + y * y
  where
    x = (x2 - x1)
    y = (y2 - y1)

vNorm :: Floating a => Vector a -> Vector a
vNorm v = v * (1/(vAbs v))


nah :: (->) (->) Float Float Float
nah a b = b*a
















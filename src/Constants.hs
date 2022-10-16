module Constants where

asteroidDespawnRange2 :: Float
asteroidDespawnRange2 =  fromIntegral (fst pageSize ^ 2 + snd pageSize ^ 2) * 3

pageSize :: (Int, Int)
pageSize = (900, 900)

fps :: Int
fps = 60

shootingInterval :: Float
shootingInterval = 0.15

bulletLifetime :: Float
bulletLifetime = 40.0

bulletSpeed :: Float
bulletSpeed = 400

bulletInitialOffset :: Float
bulletInitialOffset = 30

bulletRadius :: Float
bulletRadius = 4

playerRadius :: Float
playerRadius = 25

asteroidRadius :: Float
asteroidRadius = 7

playerAcceleration :: Float
playerAcceleration = 300

starAmount :: Int
starAmount = 4 * (uncurry (*) pageSize `div` 90000)

parallax :: Float
parallax = 0.5

playerFrictionExponent :: Float
playerFrictionExponent = 0.6

spawnDistance :: Float
spawnDistance = sqrt (fromIntegral (((fst pageSize `div` 2) ^ 2) + ((snd pageSize `div` 2) ^ 2)))
-- spawnDistance = 300
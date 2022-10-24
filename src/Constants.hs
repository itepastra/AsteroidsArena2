module Constants where

import Graphics.Gloss (Path, Picture, lineLoop, polygon)

asteroidDespawnRange2 :: Float
asteroidDespawnRange2 = fromIntegral (fst pageSize ^ 2 + snd pageSize ^ 2) * 3

pageSize :: (Int, Int)
pageSize = (1600, 900)

fps :: Int
fps = 60

shootingInterval :: Float
shootingInterval = 0.25

bulletLifetime :: Float
bulletLifetime = 5

bulletSpeed :: Float
bulletSpeed = 400

bulletInitialOffset :: Float
bulletInitialOffset = 40

bulletRadius :: Float
bulletRadius = 4

playerRadius :: Float
playerRadius = 25

asteroidRadius :: Float
asteroidRadius = 7

playerAcceleration :: Float
playerAcceleration = 300

starAmount :: [Int]
starAmount = map (* (uncurry (*) pageSize `div` 90000)) [1 ..]

parallaxStart :: Int
parallaxStart = 2

parallaxLayers :: Int
parallaxLayers = 5

parallax :: [Float]
parallax = map ((1 /) . fromIntegral) [parallaxStart .. (parallaxStart + parallaxLayers)]

playerFrictionExponent :: Float
playerFrictionExponent = 0.6

spawnDistance :: Float
spawnDistance = sqrt (fromIntegral (((fst pageSize `div` 2) ^ 2) + ((snd pageSize `div` 2) ^ 2)))

asteroidSpawnAverageInterval :: Float
asteroidSpawnAverageInterval = 5

playerRotateSpeed :: Float
playerRotateSpeed = 180

babyAsteroidMinimumSpeed :: Float
babyAsteroidMinimumSpeed = 10

babyAsteroidMaximumSpeed :: Float
babyAsteroidMaximumSpeed = 50

babyAsteroidMinimumRotation :: Float
babyAsteroidMinimumRotation = -10

babyAsteroidMaximumRotation :: Float
babyAsteroidMaximumRotation = 10



module Constants where

import GeneralHelperFunctions (biF)

pageSize :: (Int, Int)
pageSize = (1600, 900)

fps :: Int
fps = 30

-- Background Things

starAmount :: [Int]
starAmount = map (* (uncurry (*) pageSize `div` 90000)) [1 ..]

parallaxStart :: Int
parallaxStart = 2

parallaxLayers :: Int
parallaxLayers = 3

parallax :: [Float]
parallax = map ((1 /) . fromIntegral) [parallaxStart .. (parallaxStart + parallaxLayers)]

-- Asteroid Things

centreToCorner2 :: Float
centreToCorner2 = (\(x, y) -> fromIntegral (x + y)) . biF (\x -> (x `div` 2) ^ 2) $ pageSize

asteroidDespawnRange2 :: Float
asteroidDespawnRange2 = centreToCorner2 * 4

asteroidRadius :: Float
asteroidRadius = 10

spawnDistance :: Float
spawnDistance = (1.5 *) . sqrt $ centreToCorner2

asteroidSpawnAverageInterval :: Float
asteroidSpawnAverageInterval = 3

babyAsteroidMinimumSpeed :: Float
babyAsteroidMinimumSpeed = 10

babyAsteroidMaximumSpeed :: Float
babyAsteroidMaximumSpeed = 50

babyAsteroidMinimumRotation :: Float
babyAsteroidMinimumRotation = -10

babyAsteroidMaximumRotation :: Float
babyAsteroidMaximumRotation = 10

asteroidFrictionExponent :: Float
asteroidFrictionExponent = 0.5

spaceMineOdds :: Float
spaceMineOdds = 0.1

-- Player Things

shootingInterval :: Float
shootingInterval = 0.25

playerRadius :: Float
playerRadius = 25

playerAcceleration :: Float
playerAcceleration = 300

playerFrictionExponent :: Float
playerFrictionExponent = 0.6

playerRotateSpeed :: Float
playerRotateSpeed = 180

playerMaxHp :: Float
playerMaxHp = 100

-- Bullet Things

bulletLifetime :: Float
bulletLifetime = 5

bulletSpeed :: Float
bulletSpeed = 400

bulletInitialOffset :: Float
bulletInitialOffset = 40

bulletRadius :: Float
bulletRadius = 4

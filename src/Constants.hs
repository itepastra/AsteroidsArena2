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

starAmount :: [Int]
starAmount = map (*  (uncurry (*) pageSize `div` 90000))  [1..]

parallax :: [Float]
parallax = [0.333, 0.25, 0.20, 0.16666, 0.1428]


playerFrictionExponent :: Float
playerFrictionExponent = 0.6

spawnDistance :: Float
spawnDistance = sqrt (fromIntegral (((fst pageSize `div` 2) ^ 2) + ((snd pageSize `div` 2) ^ 2)))

asteroidSpawnAverageInterval :: Float
asteroidSpawnAverageInterval = 0.5

playerRotateSpeed :: Float
playerRotateSpeed = 180

babyAsteroidMinimumSpeed :: Float
babyAsteroidMinimumSpeed = 10

babyAsteroidMaximumSpeed :: Float
babyAsteroidMaximumSpeed = 50
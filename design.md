# Introduction and Rules

Asteroids Arena is a real time game where the player flies a spacecraft through an asteroid field in an arena. To survive the player has to shoot approaching asteroids.  
The arena starts big, but slowly becomes smaller and smaller.  
The player must not get hit by asteroids, their own bullets or the walls. For if they do, they die
The walls don't destroy bullets, instead the shots get reflected.  
To get a high score the player must survive and destroy asteroids as long as possible.
The game is finished when the players lives are up and the player gets hit.

# Design

## Data Structures

### The Game State

The full state of our game is represented using a GameState data type:
>``` 
> data GameState = GameState { player       :: PlayerType 
>                            , score        :: Integer 
>                            , asteroids    :: [AsteroidType] 
>                            , bullets      :: [BulletType]
>                            , walls        :: [WallType]
>                            } 
>``` 

### The Player and bullets

>``` 
> data PlayerType = PlayerType { lives      :: Int
>                            , position     :: (Double, Double) 
>                            , velocity     :: (Double, Double) 
>                            , bullets      :: [BulletType]
>                            , walls        :: [WallType]
>                            } 
>``` 

### The Asteroids

### The field


### Player Movement


## Interface
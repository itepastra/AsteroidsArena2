# Asteroids Arena

## Introduction and Rules

Asteroids Arena is a real-time game where the player flies a spacecraft through an asteroid field in an arena. To survive the player has to shoot or avoid approaching asteroids.  
Unlike normal asteroids with a stationary camera, our game will have a camera following the player, being limited in the space by an arena. 
The arena starts big, but slowly gets smaller and smaller.  
The player must not get hit by asteroids or their own bullets, for if they do, they die.
Should the player hit a wall, they "bounce" off and get forced back into the shrinking arena.
The walls do not destroy bullets, instead the shots get "bounced" off like the player.  
To get a high score the player must survive as long as possible, while destroying as many asteroids as they can.
The game is finished when the players is out of lives and the player gets hit.
The brackets in this document mean we are not yet certain whether we will have time to implement this.

## Design

### Data Structures

#### The Game State

The full state of our game is represented using a GameState data type:  

>``` 
> data GameState = GameState { player       :: Player 
>                            , score        :: Integer 
>                            , asteroids    :: [Asteroid] 
>                            , bullets      :: [Bullet]
>                            , walls        :: [Wall]
>                            } 
>```   

#### Physics

We have several data types to help us with the physics in our game.

>```
> type Vector = Point
> type TimeStep = Float
> type Position = Point
> type Acceleration = Vector
> type Velocity = Vector
> type Dist = Float
> type Collides = Bool
>
> data PhysicsObject = PhysObj { position :: Position
>                              , velocity :: Velocity
>                              , radius :: Float 
>                              }
>```

#### Player

These are the data types used for the player.

>```
> type Lives = Int
> type LookDirection = Vector
>
> data Player = Player PhysicsObject Lives LookDirection
>```

#### Bullet

These are the data types used for the bullets.

>```
> type Lifetime = Float
>
> data Bullet = Bullet PhysicsObject Lifetime
>```

#### Asteroid 

These are the data types used for the asteroids.

>```
> type Size = Int
>
> data Asteroid = Asteroid PhysicsObject Size
>```

#### Wall

These are the data types used for the walls.

>```
> type Normal = Vector
> type InWall = Bool
> type Strength = Float
>
> data Wall = Wall { point :: Point
>                  , normal :: Normal
>                  , strength :: Strength
>                  }
>```

## Minimum Requirements

### Player

The player controls the spaceship, being able to accelerate (w), turn left (a) and turn right (d). They will also be able to shoot (spacebar).

### Enemies

Asteroids get spawned with slightly random direction and size. There is also the shrinking walls making the arena smaller. The bullets the player shoots themselves are also a type of enemy, as they can kill both asteroids and the player.  
(Turrets spawning on asteroids, turrets on the walls, UFOs)

### Randomness

The randomness stems from the random size, direction and spawn location of the asteroids, within reason. Meaning that the asteroids should not all be in the same place, or move away from the player too far. It also should not happen that the asteroids get too big and too many that the game becomes impossible. When an asteroid of larger size explodes, its babies should go in random directions.

### Animation

When the player dies, the spaceship explodes. When an asteroid is shot, the asteroid explodes. 

### Pause

The game is paused when a specific button is pressed (p) 

### Interaction with the File System

A list of high scores will be saved in an external file. There will also be different levels put into seperate files.

## Optional Requirements

### Custom levels / levels
(Change the different walls and how they move.) 

### Enemies
(Turrets on walls as well as turrets on asteroids, in combination with normal asteroids, walls and bullets).

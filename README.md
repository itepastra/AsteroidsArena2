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

## Design

### Data Structures

#### The Game State

The full state of our game is represented using a GameState data type:  

>``` 
> data GameState = GameState { player       :: PhysicsObject Double 
>                            , score        :: Integer 
>                            , asteroids    :: [PhysicsObject Double] 
>                            , bullets      :: [PhysicsObject Double]
>                            , walls        :: [WallType]
>                            } 
>```   

#### Physics Objects

>``` 
> data ObjectType a = Player Lives (Vector a) | Asteroid Size | Bullet Lifespan
>
> data PhysicsObject a = PhysObj {
>    ot :: ObjectType,
>    position :: Position a,
>    velocity :: Velocity a,
>    radius :: a
>                              }
>```

#### The Asteroids

#### The Field

#### Player Movement

### Interface

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

A list of high scores will be saved in an external file.  
(List of levels)

## Optional Requirements

### Custom levels / levels
(Change the different walls and how they move.) 

### Enemies
(Turrets on walls as well as turrets on asteroids, in combination with normal asteroids, walls and bullets).
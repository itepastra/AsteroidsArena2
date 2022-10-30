{-# LANGUAGE OverloadedStrings #-}

module JSONfuncs where

import Asteroid (Asteroid (..))
import Bullet (Bullet (..))
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), object, withObject, (.:))
import Model (GameState (GameState), GameStateInit (..), Level (..))
import Physics (PhysicsObject (PhysObj, position, radius, velocity))
import Player (Player (..))
import TypeClasses (V2Math (..))
import VectorCalc (Point (Point))
import Wall (Wall (..))
import Level (LevelConfig(..))

instance FromJSON Point where
  parseJSON = withObject "Point" $ \v ->
    Point
      <$> v
      .: "x"
      <*> v
      .: "y"

instance ToJSON Point where
  toJSON p =
    object
      [ "x" .= x p,
        "y" .= y p
      ]

instance FromJSON PhysicsObject where
  parseJSON = withObject "PhysicsObject" $ \v ->
    PhysObj
      <$> v
      .: "position"
      <*> v
      .: "velocity"
      <*> v
      .: "radius"

instance ToJSON PhysicsObject where
  toJSON p =
    object
      [ "position" .= position p,
        "velocity" .= velocity p,
        "radius" .= radius p
      ]

instance FromJSON Player where
  parseJSON = withObject "Player" $ \v ->
    Player
      <$> v
      .: "phys"
      <*> v
      .: "hp"
      <*> v
      .: "lookDirection"
      <*> v
      .: "lookAngle"

instance ToJSON Player where
  toJSON p =
    object
      [ "phys" .= Player.phys p,
        "hp" .= hp p,
        "lookDirection" .= lookDirection p,
        "lookAngle" .= lookAngle p
      ]

instance FromJSON Asteroid where
  parseJSON = withObject "Asteroid" $ \v ->
    Asteroid
      <$> v
      .: "phys"
      <*> v
      .: "size"
      <*> v
      .: "rotateSpeed"
      <*> v
      .: "rotateAngle"

instance ToJSON Asteroid where
  toJSON p =
    object
      [ "phys" .= Asteroid.phys p,
        "size" .= size p,
        "rotateSpeed" .= rotateSpeed p,
        "rotateAngle" .= rotateAngle p
      ]

instance FromJSON Bullet where
  parseJSON = withObject "Bullet" $ \v ->
    Bullet
      <$> v
      .: "phys"
      <*> v
      .: "lifeTime"

instance ToJSON Bullet where
  toJSON p =
    object
      [ "phys" .= Bullet.phys p,
        "lifeTime" .= lifeTime p
      ]

instance FromJSON Level where
  parseJSON = withObject "Level" $ \v ->
    Level <$> v .: "name" <*> v .: "initState"

instance ToJSON Level where
  toJSON l =
    object
      [ "name" .= name l,
        "initState" .= initState l
      ]

instance FromJSON Wall where
  parseJSON = withObject "Wall" $ \v ->
    Wall
      <$> v
      .: "point"
      <*> v
      .: "normal"
      <*> v
      .: "strength"
      <*> v
      .: "angle"

instance ToJSON Wall where
  toJSON w =
    object
      [ "point" .= point w,
        "normal" .= normal w,
        "strength" .= strength w,
        "angle" .= angle w
      ]

instance FromJSON LevelConfig where
  parseJSON = withObject "LevelConfig" $ \v ->
    LevelConfig
      <$> v
      .: "asteroidSpawnFunction"

instance ToJSON LevelConfig where
  toJSON w =
    object
      [ "asteroidSpawnFunction" .= asteroidSpawnFunction w
      ]

instance FromJSON GameStateInit where
  parseJSON = withObject "GameStateInit" $ \v ->
    GameStateInit
      <$> v
      .: "initWalls"
      <*> v
      .: "initConf"

instance ToJSON GameStateInit where
  toJSON w =
    object
      [ "initWalls" .= initWalls w,
        "initConf" .= initConf w
      ]

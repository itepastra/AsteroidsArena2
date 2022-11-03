{-# LANGUAGE OverloadedStrings #-}

module JSONfuncs where

import Asteroid (Asteroid (..))
import AsteroidSpawnFunctions (RandomFunctions (..))
import Bullet (Bullet (..))
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), object, withObject, (.:))
import Level (GameStateInit (..), InitLevelConfig (..), Level (..), LevelConfig (..))
import Model (GameState (GameState))
import Physics (PhysicsObject (PhysObj, position, radius, velocity))
import Player (Player (..))
import TypeClasses (V2Math (..))
import VectorCalc (Point (Point))
import Wall (InitWall (..), Wall (..))
import WallFunctions (WallFunction (..))
import Control.Monad (MonadPlus(mzero))
import Control.Applicative ((<|>))
import qualified Data.Aeson.KeyMap as H

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

instance FromJSON InitWall where
  parseJSON = withObject "InitWall" $ \v ->
    InitWall
      <$> v
      .: "irFunc"
      <*> v
      .: "ioFunc"
      <*> v
      .: "isFunc"

instance ToJSON InitWall where
  toJSON w =
    object
      [ "ioFunc" .= ioFunc w,
        "irFunc" .= irFunc w,
        "isFunc" .= isFunc w
      ]

instance FromJSON InitLevelConfig where
  parseJSON = withObject "InitLevelConfig" $ \v ->
    InitLevelConfig
      <$> v
      .: "asteroidSpawnFunction"
      <*> v
      .: "asteroidDecayFunction"
      <*> v
      .: "spaceMineOddsFunction"
      <*> v
      .: "asteroidSpawnStart"

instance ToJSON InitLevelConfig where
  toJSON w =
    object
      [ "asteroidSpawnFunction" .= iasteroidSpawnFunction w,
        "asteroidDecayFunction" .= iasteroidDecayFunction w,
        "spaceMineOddsFunction" .= ispaceMineOddsFunction w,
        "asteroidSpawnStart" .= iasteroidSpawnStart w
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

instance FromJSON Level where
  parseJSON = withObject "Level" $ \v ->
    Level <$> v .: "name" <*> v .: "initState"

instance ToJSON Level where
  toJSON l =
    object
      [ "name" .= name l,
        "initState" .= initState l
      ]

instance ToJSON WallFunction where
  toJSON (C v) = object ["type" .= ("c" :: String), "v" .= v]
  toJSON Etime = object ["type" .= ("etime" :: String)]
  toJSON (SinF f1) = object ["type" .= ("sin" :: String), "f1" .= f1]
  toJSON (LinF f1 f2) = object ["type" .= ("lin" :: String), "f1" .= f1, "f2" .= f2]
  toJSON (ExpF f1 f2) = object ["type" .= ("exp" :: String), "f1" .= f1, "f2" .= f2]
  toJSON (AddF f1 f2) = object ["type" .= ("add" :: String), "f1" .= f1, "f2" .= f2]

instance FromJSON WallFunction where
  parseJSON  = withObject "WallFunction" $ \v ->
    case H.lookup "type" v of
      Nothing -> mzero
      Just "c" -> C <$> v .: "v"
      Just "etime" -> return Etime
      Just "lin" -> LinF <$> v.: "f1" <*> v.: "f2"
      Just "exp" -> ExpF <$> v.: "f1" <*> v.: "f2"
      Just "add" -> AddF <$> v.: "f1" <*> v.: "f2"
      Just "sin" -> SinF <$> v.: "f1"
      _ -> mzero
{-# LANGUAGE OverloadedStrings #-}

module JSONfuncs where

import Asteroid (Asteroid (..))
import Bullet (Bullet (..))
import Control.Applicative ((<|>))
import Control.Monad (MonadPlus (mzero))
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), object, withObject, (.:))
import qualified Data.Aeson.KeyMap as A
import Level (GameStateInit (..), InitLevelConfig (..), Level (..), LevelConfig (..))
import Model (GameState (GameState))
import Player (Player (..))
import qualified Player
import Types1 (PhysicsObject (..), Point (Point), Var (..))
import VFunctions (DOp (..), SOp (..), VFunction (..))
import Wall (InitWall (..), Wall (..))
import VectorCalc ( V2Math(y, x) )

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
      .: "spaceMineOddsFunction"
      <*> v
      .: "asteroidSpawnStart"

instance ToJSON InitLevelConfig where
  toJSON w =
    object
      [ "asteroidSpawnFunction" .= iasteroidSpawnFunction w,
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

instance (ToJSON a, ToJSON b, Show b) => ToJSON (VFunction b a) where
  toJSON (Constant v) = object ["_type" .= ("c" :: String), "v" .= v]
  toJSON (Variable x) = object ["_type" .= ("var" :: String), "v" .= x]
  toJSON (OneIn op f1) = object ["_type" .= ("sfunc" :: String), "_op" .= op, "f1" .= f1]
  toJSON (TwoIn op f1 f2) = object ["_type" .= ("dfunc" :: String), "_op" .= op, "f1" .= f1, "f2" .= f2]
  toJSON (ThreeIn op f1 f2 f3) = object ["_type" .= ("dfunc" :: String), "_op" .= op, "f1" .= f1, "f2" .= f2, "f3" .= f3]

instance (FromJSON a, FromJSON b, Read b) => FromJSON (VFunction b a) where
  parseJSON = withObject "AFunction" $ \v ->
    case A.lookup "_type" v of
      Nothing -> mzero
      Just "c" -> Constant <$> v .: "v"
      Just "var" -> Variable <$> v .: "v"
      Just "sfunc" -> OneIn <$> v .: "_op" <*> v .: "f1"
      Just "dfunc" -> TwoIn <$> v .: "_op" <*> v .: "f1" <*> v .: "f2"
      _ -> mzero

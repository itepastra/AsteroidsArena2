{-# LANGUAGE OverloadedStrings #-}

module JSONfuncs where

import Asteroid (Asteroid (..))
import AsteroidSpawnFunctions (RandomFunctions (..))
import Bullet (Bullet (..))
import Control.Applicative ((<|>))
import Control.Monad (MonadPlus (mzero))
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), object, withObject, (.:))
import qualified Data.Aeson.KeyMap as A
import Level (GameStateInit (..), InitLevelConfig (..), Level (..), LevelConfig (..))
import Model (GameState (GameState))
import Player (Player (..))
import qualified Player
import TypeClasses (V2Math (..))
import Types1
  (
    PhysicsObject (..),
    Point (Point),
  )
import Wall (InitWall (..), Wall (..))
import AFunctions (AFunction(..))

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

instance (ToJSON a) => ToJSON (AFunction a) where
  toJSON (C v) = object ["type" .= ("c" :: String), "v" .= v]
  toJSON Var = object ["type" .= ("etime" :: String)]
  toJSON (SinF f1) = object ["type" .= ("sin" :: String), "f1" .= f1]
  toJSON (MulF f1 f2) = object ["type" .= ("lin" :: String), "f1" .= f1, "f2" .= f2]
  toJSON (AddF f1 f2) = object ["type" .= ("add" :: String), "f1" .= f1, "f2" .= f2]
  toJSON (SubF f1 f2) = object ["type" .= ("sub" :: String), "f1" .= f1, "f2" .= f2]
  toJSON (DivF f1 f2) = object ["type" .= ("div" :: String), "f1" .= f1, "f2" .= f2]
  toJSON (AbsF f1) = object ["type" .= ("abs" :: String), "f1" .= f1]
  toJSON (SigF f1) = object ["type" .= ("sig" :: String), "f1" .= f1]
  toJSON (ExpF f1) = object ["type" .= ("exp" :: String), "f1" .= f1]
  toJSON (LogF f1) = object ["type" .= ("log" :: String), "f1" .= f1]
  toJSON (CosF f1) = object ["type" .= ("cos" :: String), "f1" .= f1]
  toJSON (AsinF f1) = object ["type" .= ("asin" :: String), "f1" .= f1]
  toJSON (AcosF f1) = object ["type" .= ("acos" :: String), "f1" .= f1]
  toJSON (AtanF f1) = object ["type" .= ("atan" :: String), "f1" .= f1]
  toJSON (AsinhF f1) = object ["type" .= ("asinh" :: String), "f1" .= f1]
  toJSON (AcoshF f1) = object ["type" .= ("acosh" :: String), "f1" .= f1]
  toJSON (AtanhF f1) = object ["type" .= ("atanh" :: String), "f1" .= f1]

instance (FromJSON a) => FromJSON (AFunction a) where
  parseJSON = withObject "AFunction" $ \v ->
    case A.lookup "type" v of
      Nothing -> mzero
      Just "c" -> C <$> v .: "v"
      Just "etime" -> return Var
      Just "lin" -> MulF <$> v .: "f1" <*> v .: "f2"
      Just "add" -> AddF <$> v .: "f1" <*> v .: "f2"
      Just "sub" -> SubF <$> v .: "f1" <*> v .: "f2"
      Just "div" -> DivF <$> v .: "f1" <*> v .: "f2"
      Just "abs" -> AbsF <$> v .: "f1"
      Just "sig" -> SigF <$> v .: "f1"
      Just "exp" -> ExpF <$> v .: "f1"
      Just "log" -> LogF <$> v .: "f1"
      Just "sin" -> SinF <$> v .: "f1"
      Just "cos" -> CosF <$> v .: "f1"
      Just "asin" -> AsinF <$> v .: "f1"
      Just "acos" -> AcosF <$> v .: "f1"
      Just "atan" -> AtanF <$> v .: "f1"
      Just "asinh" -> AsinhF <$> v .: "f1"
      Just "acosh" -> AcoshF <$> v .: "f1"
      Just "atanh" -> AtanhF <$> v .: "f1"
      _ -> mzero
{-# LANGUAGE OverloadedStrings #-}

module Bullet where

import Graphics.Gloss (translate)
import JSONfuncs
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    object,
    withObject,
    (.:),
    (.=),
  )
import Physics ()
import Pictured (Pictured (..), mvWithPhys)
import Sprites (baseBullet)
import TypeClasses (HasPhysics (..))
import Types1 (Lifetime, PhysicsObject (..), TimeStep)

data Bullet = Bullet
  { phys :: PhysicsObject,
    lifeTime :: Lifetime
  }

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

instance HasPhysics Bullet where
  getPhysObj (Bullet p _) = p
  setPhysObj po a = a {phys = po}

instance Pictured Bullet where
  getPicture o@(Bullet {}) = mvWithPhys o (baseBullet (lifeTime o))

updateLifetime :: TimeStep -> Bullet -> Bullet
updateLifetime ts (Bullet p l) = Bullet p (l - ts)

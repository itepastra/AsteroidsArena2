{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types1 where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Point (Point (Point))

type Time = Float

type TimeStep = Time -- a timestep between frames

type UniformTime = Time -- a uniformly distributed time between 0 and 1

type ElapsedTime = Time -- the time since an event started

type IntervalTime = Time -- time till another event should happen

type TimeAvg = Time -- an average timeinterval a function returns when a uniform time is given

type Lifetime = Time -- how much longer a bullet should exist

type Angle = Float -- an angle in degrees (0 degrees is in the positive y direction, rotating clockwise)

type Vector = Point Float

type Position = Point Float

type Acceleration = Vector

type Velocity = Vector

type Dist = Float -- a distance between two points

type Collides = Bool -- wether two hitboxes overlap

type HealthPoints = Float -- the health of the player

type LookDirection = Vector -- a normalized vector in the direction the player is looking

type Normal = Vector -- a normalized vector normal to a wall

type Strength = Float -- how strongly a wall pushes the player and bullets

type InWall = Bool -- wether an object should get forces from the wall

type Size = Word -- how large an asteroid is

type Offset = Float -- how far from the origin a wall is

type AngleSpeed = Angle -- the change in angle per second

type Score = Int

type FunctionString = String -- a representation for a function

data Hud = Visible | Invisible -- if the HUD should be drawn in the screen

data Selected a = NotSelected {val :: a} | Selected {time :: ElapsedTime, val :: a} -- whether options in the menu are selected or not

data Part = Str | Rot | Off

data OverlayText = OT String | ST String

data Var = X | Y | Z | I | J | K
  deriving (Eq, Ord, Show, Read, ToJSON, FromJSON, Generic)

data PhysicsObject = PhysObj
  { position :: Position,
    velocity :: Velocity,
    radius :: Float
  }

instance Bounded UniformTime where
  minBound = 0
  maxBound = 1

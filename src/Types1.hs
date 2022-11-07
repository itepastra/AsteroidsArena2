module Types1 where

data Point = Point Float Float

type Angle = Float

type Vector = Point

type Time = Float

type TimeStep = Time

type UniformTime = Time

type ElapsedTime = Time

type IntervalTime = Time

type TimeAvg = Time

type Position = Point

type Acceleration = Vector

type Velocity = Vector

type Dist = Float

type Collides = Bool

type HealthPoints = Float

type LookDirection = Vector

type Normal = Vector

type Strength = Float

type InWall = Bool

type Size = Int

type Lifetime = Float

type Offset = Float

type AngleSpeed = Angle

type Decay = Float

data Selected a = NotSelected {val :: a} | Selected {time :: ElapsedTime, val :: a}

data Hud = Visible | Invisible

instance Eq a => Eq (Selected a) where
  s1 == s2 = val s1 == val s2

instance Ord a => Ord (Selected a) where
  compare s1 s2 = compare (val s1) (val s2)
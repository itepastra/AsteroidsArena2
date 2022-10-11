data Vector a = Vector a a

type Position = Vector

type Velocity = Vector

type Lives = Int

type Size = Int

type Lifetime = Double

type Normal a = Vector a

type Dist a = a


data PhysicsObject a = PhysObj
  { position :: Position a,
    velocity :: Velocity a,
    radius :: a
  }


data Player a = Player Lives  (Vector a) (PhysicsObject a)
data Asteroid a = Asteroid Size (PhysicsObject a)
data Bullet a = Bullet Lifetime (PhysicsObject a)




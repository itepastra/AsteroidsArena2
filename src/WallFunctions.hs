-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}

module WallFunctions where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Types1 (Decay, ElapsedTime)

data WallFunction = LinF WallFunction WallFunction | ExpF WallFunction WallFunction | AddF WallFunction WallFunction | C Float | Etime | SinF WallFunction
  -- deriving (Generic, ToJSON, FromJSON)

createFunc :: WallFunction -> ElapsedTime -> Float
createFunc (C v) _ = v
createFunc Etime et = et
createFunc (LinF fa fb) et = createFunc fa et * createFunc fb et
createFunc (ExpF fa fb) et = createFunc fa et ** createFunc fb et
createFunc (AddF fa fb) et = createFunc fa et + createFunc fb et
createFunc (SinF fa) et = sin (createFunc fa et)

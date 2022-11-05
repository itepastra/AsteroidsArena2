-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}

module WallFunctions where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Types1 (Decay, ElapsedTime)
import GHC.Read (Read(readPrec))

data WallFunction = LinF WallFunction WallFunction | ExpF WallFunction WallFunction | AddF WallFunction WallFunction | C Float | Etime | SinF WallFunction


createFunc :: WallFunction -> ElapsedTime -> Float
createFunc (C v) _ = v
createFunc Etime et = et
createFunc (LinF fa fb) et = createFunc fa et * createFunc fb et
createFunc (ExpF fa fb) et = createFunc fa et ** createFunc fb et
createFunc (AddF fa fb) et = createFunc fa et + createFunc fb et
createFunc (SinF fa) et = sin (createFunc fa et)


toString :: WallFunction -> String
toString (C v) = show v
toString Etime = "E"
toString (LinF fa fb) = "(" ++ toString fa ++ ")*("++ toString fb++")"
toString (AddF fa fb) = "(" ++ toString fa ++ ")+("++ toString fb++")"
toString (ExpF fa fb) = "(" ++ toString fa ++ ")**("++ toString fb++")"
toString (SinF fa) = "sin("++ toString fa ++ ")"

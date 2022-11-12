{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module InitWall where
import VFunctions (VFunction)
import Types1 (Var)
import TypeClasses (HasA (..))

data InitWall = InitWall
  { irFunc :: VFunction Float Var,
    ioFunc :: VFunction Float Var,
    isFunc :: VFunction Float Var
  }
instance Show InitWall where
  show w = "Rotation: " ++ show (irFunc w) ++ " Offset: " ++ show (ioFunc w) ++ " Strength: " ++ show (isFunc w) ++ "\n"

instance HasA (VFunction Float Var, VFunction Float Var, VFunction Float Var) InitWall where
  getA w = (irFunc w, ioFunc w, isFunc w)
  setA (irf, iof, isf) iw = iw {irFunc = irf, ioFunc = iof, isFunc = isf}

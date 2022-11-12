{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module InitWall where

import Data.Map (fromList)
import Rotation (Rotate (..))
import TypeClasses (HasA (..))
import Types1 (Var (X))
import VFunctions (VFunction (Constant), mkNumFunc)

data InitWall = InitWall
  { irFunc :: VFunction Float Var,
    ioFunc :: VFunction Float Var,
    isFunc :: VFunction Float Var
  }

instance Show InitWall where
  show w = "Rotation: " ++ show (irFunc w) ++ " Offset: " ++ show (ioFunc w) ++ " Strength: " ++ show (isFunc w) ++ "\n"

instance Rotate InitWall where
  rotate a iw = iw {irFunc = Constant a}
  getAngle iw = mkNumFunc (irFunc iw) (fromList [(X, 0)])

instance HasA (VFunction Float Var, VFunction Float Var, VFunction Float Var) InitWall where
  getA w = (irFunc w, ioFunc w, isFunc w)
  setA (irf, iof, isf) iw = iw {irFunc = irf, ioFunc = iof, isFunc = isf}

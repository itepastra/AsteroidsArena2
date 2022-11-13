{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module InitWall where

import Data.Map (fromList)
import Rotation (Rotate (..))
import TypeClasses (HasA (..))
import Types1 (Var (X))
import VFunctions (VFunction (Constant), mkNumFunc)
import JSONfuncs

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
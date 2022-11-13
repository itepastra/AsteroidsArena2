module EditorModel where

import Data.List (intercalate)
import Data.Set (Set)
import GeneralHelperFunctions (scaleboth, translateP)
import Graphics.Gloss.Interface.IO.Game (Key, circleSolid, color, green)
import Level (GameStateInit (GameStateInit, initConf, initWalls), InitLevelConfig (InitLevelConfig), Level (Level))
import Pictured (Picture (..), Pictured (..), rotWithRot, scale, textFormat, translate)
import Rotation (Rotate (getAngle))
import Select (getAllSelected)
import Sprites (baseWall, selectedWall)
import Types1 (ElapsedTime, Selected (..))
import Wall (InitWall (InitWall), createWall, point, selfMove)

data EditorState = CreatorState
  { elapsedTime :: ElapsedTime,
    iwalls :: [Selected InitWall],
    keys :: Set Key,
    ilevelConfig :: InitLevelConfig,
    lname :: String,
    timeMultiplier :: Float
  }

levelFromCreatorState :: EditorState -> Level
levelFromCreatorState gs = Level (lname gs) GameStateInit {initWalls = map val $ iwalls gs, initConf = ilevelConfig gs}

instance Pictured EditorState where
  getPicture gs =
    Pictures
      [ viewWallsSelect (elapsedTime gs) (iwalls gs), -- display the walls
        color green $ circleSolid 20, -- display where the player is
        textFormat (-800) 400 0.2 (intercalate "\n" $ map show $ getAllSelected $ iwalls gs), -- display the wall stats
        textFormat (-800) (-430) 0.2 (show (elapsedTime gs) ++ " @ " ++ show (timeMultiplier gs))
      ]

viewWallsSelect :: ElapsedTime -> [Selected InitWall] -> Picture
viewWallsSelect et = Pictures . map (viewSelectedWall et)

viewSelectedWall :: ElapsedTime -> Selected InitWall -> Picture
viewSelectedWall et iw = case iw of
  NotSelected iw' -> translateP (p iw') $ rotWithRot iw' baseWall
  Selected _ iw' -> translateP (p iw') $ rotWithRot iw' selectedWall
  where
    p i = point (w i)
    a i = getAngle (w i)
    w iw'' = selfMove et $ createWall iw''

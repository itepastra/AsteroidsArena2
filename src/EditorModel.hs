module EditorModel where
import Types1 (ElapsedTime, Selected (..))
import Wall (InitWall(InitWall), point, selfMove, createWall)
import Data.Set (Set)
import Level (InitLevelConfig(InitLevelConfig), Level (Level), GameStateInit (GameStateInit, initWalls, initConf))
import Pictured (Picture(..), Pictured (..), scale, colorText, translate, rotWithRot)
import Data.List (intercalate)
import Select (getAllSelected)
import Graphics.Gloss.Interface.IO.Game (Key, circleSolid, color, green)
import Sprites (baseWall, selectedWall)
import VectorCalc (V2Math(y), x)
import Rotation
import GeneralHelperFunctions (scaleboth)

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
        translate (-800) 400 $ colorText $ scaleboth 0.2 $ Text (intercalate "\n" $ map show $ getAllSelected $ iwalls gs), -- display the wall stats
        translate (-800) (-430) $ colorText $ scaleboth 0.2 $ Text (show (elapsedTime gs) ++ " @ " ++ show (timeMultiplier gs) )
      ]


viewWallsSelect :: ElapsedTime -> [Selected InitWall] -> Picture
viewWallsSelect et = Pictures . map (viewSelectedWall et)

viewSelectedWall :: ElapsedTime -> Selected InitWall -> Picture
viewSelectedWall et iw = case iw of
  NotSelected iw' -> translate (x (p iw')) (y (p iw')) $ rotWithRot iw' baseWall
  Selected _ iw' -> translate (x (p iw')) (y (p iw')) $ rotWithRot iw' selectedWall
  where
    p i = point (w i)
    a i = getAngle (w i)
    w iw'' = selfMove et $ createWall iw''

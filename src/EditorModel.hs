module EditorModel where
import Types1 (ElapsedTime, Selected (..))
import Wall (InitWall(InitWall))
import Graphics.Gloss.Interface.IO.Game (Key)
import Data.Set (Set)
import Level (InitLevelConfig(InitLevelConfig), Level (Level), GameStateInit (GameStateInit, initWalls, initConf))

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

module CoordCalc where
import qualified Constants
import GHC.Float (float2Int)


screenToWorld :: Int -> Float
screenToWorld a = fromIntegral a / fromIntegral (fst Constants.pageSize)


worldToScreen :: Float -> Int
worldToScreen a = float2Int a * fst Constants.pageSize


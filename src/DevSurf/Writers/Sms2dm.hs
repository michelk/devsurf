module DevSurf.Writers.Sms2dm (renderSms2dm) where
import DevSurf.Types
import Data.HashMap.Lazy (toList)
import Data.List (intersperse)
import DevSurf.Writers.Utils (toOrdList)

renderSms2dm :: IndexedFaceSet -> String
renderSms2dm (IndexedFaceSet fs vs) = unlines . concat $ [
    ["MESH2D"]
   , map renderFace (toOrdList fs)
   , map renderVertex (toOrdList vs)
   ]

renderFace :: (Int,IndexedFace) -> String
renderFace (i, (v1,v2,v3)) =
    "E3T " ++ " "++ show i ++ " " ++ (showIntList [v1,v2,v3]) ++ " 1"

renderVertex :: (Int,Vertex) -> String
renderVertex (i,(x,y,z)) =
    "ND " ++ show i ++ " " ++ (showDoubleList [x,y,z])

showDoubleList :: [Double] -> String
showDoubleList = unwords . intersperse " " . map show

showIntList :: [Int] -> String
showIntList = unwords . intersperse " " . map show

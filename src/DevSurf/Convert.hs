module DevSurf.Convert (fsToIfs) where
import DevSurf.Types
import qualified Data.HashMap.Lazy as H
import Data.List (intercalate, nub)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

-- | Convert a 'FaceSet' to an 'IndexedFaceSet'
fsToIfs :: FaceSet -> IndexedFaceSet
fsToIfs fs = IndexedFaceSet fcsMap ndsMap
  where
    ndsStrMap :: H.HashMap String Int
    ndsStrMap = H.fromList $ zip (nodeListStrings fs) [1..]
    fcs :: [IndexedFace]
    fcs = map (lookupFace ndsStrMap) fs
    fcsMap = H.fromList $ zip [1..] fcs
    ndsMap = H.fromList . strMapToNdsMap . H.toList $ ndsStrMap

nodeListStrings :: FaceSet -> [String]
nodeListStrings = nub . concatMap faceToStrings
   where
     faceToStrings :: Face -> [String]
     faceToStrings (v1,v2,v3) = map vertexToString [v1,v2,v3]

vertexToString :: Vertex -> String
vertexToString (x,y,z) = intercalate "_" . map show $ [x,y,z]

lookupFace :: H.HashMap String Int -> Face -> IndexedFace
lookupFace  m (v1, v2, v3) = (i1,i2,i3)
  where
   (i1:i2:i3:_) = map (lookupVertex m) [v1,v2,v3]

lookupVertex :: H.HashMap String Int -> Vertex -> Int
lookupVertex m v = fromJust $ H.lookup (vertexToString v) m

strToVertex :: String -> Vertex
strToVertex s =
  let (x:y:z:_) = map read $ splitOn "_" s
  in (x,y,z)

strMapToNdsMap :: [(String,Int)]-> [(Int, Vertex)]
strMapToNdsMap = map strToIntVertex
  where
    strToIntVertex (s,i) = (i,strToVertex s)

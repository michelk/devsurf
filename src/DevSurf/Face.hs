module DevSurf.Face (
    elements
  , centerFace
  , indexedFaceToEdges
  , indexedFaceToFace
  , indexFaceFromList
  , faceNormal
  , aspectRatioFace
  ) where
import DevSurf.Types
import DevSurf.Vector (normalize, sub, cross, magnitude)
import Data.Maybe (mapMaybe)
import qualified Data.HashMap.Lazy as M
  
-- | Convert a triangle-mesh into a list of coordinate-based-triangles 
elements :: IndexedFaceSet -> [Face]
elements (IndexedFaceSet  es ns  ) =
         M.elems $  M.map (indexedFaceToFace ns) es

-- | Convert an index-element to coord-element
indexedFaceToFace :: M.HashMap Int Vertex -> IndexedFace -> Face
indexedFaceToFace ns (i,j,k) = (a,b,c)
  where
    a:b:c:[] = mapMaybe  (index2Vertex  ns) [i,j,k]

-- | look a Vertex up in IndexMap
index2Vertex :: M.HashMap Int Vertex -> Int -> Maybe Vertex
index2Vertex nds v = M.lookup v nds

-- | Convert a list of three indices into an index-element
indexFaceFromList :: [Int] -> IndexedFace
indexFaceFromList xs@(a:b:c:[])
  | length xs /= 3 = error "List doesn't contain three indices"                
  | otherwise = (a,b,c)

-- | Calculate the aspect-ration of a face, which is the ratio between longest to shortest edge
aspectRatioFace :: Face -> Double
aspectRatioFace (v1,v2,v3) = maximum es / minimum es
    where 
      es = map magnitude [(v1 `sub` v2), (v1 `sub` v3), (v2 `sub` v3)]
aspectRatioFace _ = undefined

-- | Calculate the center of a triangle
centerFace :: Face -> Vertex
centerFace (a,b,c)  =  meanCs [a,b,c]

meanCs :: [Vertex] -> Vertex
meanCs =  foldr1 meanC 

meanC :: Vertex -> Vertex -> Vertex
meanC  (x1,x2,x3) (y1,y2,y3) = (mean x1 y1, mean x2 y2, mean x3 y3)
  where
    mean x y = (x+y)/2

-- | Computes a surface normal for a Face
faceNormal :: Face -> Vector
faceNormal (v1,v2,v3) = normalize $ a `cross` b
  where
  a = v2 `sub` v1
  b = v3 `sub` v2

indexedFaceToEdges :: IndexedFace -> [IndexedEdge]
indexedFaceToEdges (n1,n2,n3) = [(n1,n2), (n2,n3), (n1,n3)]
                                
faceArea :: Face -> Double
faceArea (v1,v2,v3) = 0.5 * magnitude ((v1 `sub` v2) `cross` (v1 `sub` v3))

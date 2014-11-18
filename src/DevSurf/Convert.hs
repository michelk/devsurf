module DevSurf.Convert where
import DevSurf.Types
import Data.HashMap.Lazy as H
import Data.List (intercalate)

fsToIfs :: FaceSet -> IndexedFaceSet
fsToIfs fs = undefined
  where
    ndsMap = nodeList fs

type NodeList = H.HashMap String Int

nodeList :: FaceSet -> NodeList
nodeList fs = undefined

vertexToString :: Vertex -> String
vertexToString (x,y,z) = intercalate "_" . Prelude.map show $ [x,y,z]

addFaceToList :: NodeList -> Face -> NodeList
addFaceToList = undefined


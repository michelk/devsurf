module DevSurf.IndexedFaceSet
       (
         getBoundaryVertices
        ,getBoundaryNbs
       )
where
import Data.HashMap.Lazy (elems, lookup, HashMap)
import Data.Maybe (fromJust)
import DevSurf.Edge (edgesToSegments, formEdgeRings)
import DevSurf.Face (indexedFaceToEdges)
import DevSurf.Types
import Prelude hiding (lookup)

-- | Extract the boundaries of an @IndexedFaceSet@
getBoundaryVertices :: IndexedFaceSet -> [[Vertex]]
getBoundaryVertices (IndexedFaceSet em nm)  = iPoliesToVerts nm bound
  where
    bound = getBoundaryNbs em

-- | Extract node-numbers of boundary-nodes
getBoundaryNbs :: HashMap Int IndexedFace -> [[Int]]
getBoundaryNbs em =  formEdgeRings out
    where
        (out,_) = edgesToSegments . concatMap indexedFaceToEdges $ elems em

iPoliesToVerts ::  HashMap Int Vertex -> [IndexedPolygon] -> [[Vertex]]
iPoliesToVerts nds = map (iPolyToVert nds)

iPolyToVert :: HashMap Int Vertex -> IndexedPolygon -> [Vertex]
iPolyToVert nds =  map (fromJust . findIndex nds )

findIndex :: HashMap Int Vertex -> Int -> Maybe Vertex
findIndex nds i = lookup i nds




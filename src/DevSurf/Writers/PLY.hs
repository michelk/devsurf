module DevSurf.Writers.PLY (renderPly)
where
import DevSurf.Types
import DevSurf.Writers.Utils (ordElems, round2Digit)
import Data.HashMap.Lazy (size)
renderPly :: IndexedFaceSet -> String
renderPly (IndexedFaceSet fs vs ) = unlines . concat $ [
    headerPly (size vs) (size fs) 
   ,map renderVertex $ (ordElems vs)
   ,map renderFace $ (ordElems fs)
   ]

-- | Creates the header of ply-file with number of vertices and number
--   of face1s
headerPly :: Int -> Int -> [String]
headerPly nVs nFs = [
   "ply"
  , "format ascii 1.0"
  , "comment VCGLIB generated"
  , "element vertex " ++ show nVs
  , "property float x"
  , "property float y"
  , "property float z"
  , "element face " ++ show nFs
  , "property list uchar int vertex_indices"
  , "end_header"
  ]

-- | Render a face; ply starts indexing with 0; by default we assume 1
--  TODO: Check for indexing
renderFace :: IndexedFace -> String
renderFace (v1,v2,v3) = 
  "3 " ++ (unwords . map (show . (subtract 1) ) $ [v1,v2,v3])

renderVertex :: Vertex -> String
renderVertex (x,y,z) = unwords . map (show . round2Digit)$ [x,y,z]


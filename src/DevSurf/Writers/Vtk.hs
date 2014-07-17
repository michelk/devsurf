module DevSurf.Writers.Vtk (renderVtk)
where
import DevSurf.Types
import DevSurf.Writers.Utils (ordElems, round2Digit)
import Data.HashMap.Lazy (size)
import Text.Printf (printf)
renderVtk :: IndexedFaceSet -> String
renderVtk (IndexedFaceSet fs vs ) = unlines [
    headerVtk
   ,renderVertices (ordElems vs)
   ,renderFaces  (ordElems fs)
   ]

-- | Creates the header of ply-file with number of vertices and number
--   of face1s
headerVtk :: String
headerVtk = unlines [
  "# vtk DataFile Version 3.0"
  ,"vtk output"
  ,"ASCII"
  ,"DATASET POLYDATA"
  ]

-- | Render a face; ply starts indexing with 0; by default we assume 1
--  TODO: Check for indexing
renderFace :: IndexedFace -> String
renderFace (v1,v2,v3) =
  "3 " ++ (unwords . map (show . (subtract 1) ) $ [v1,v2,v3])

renderFaces :: [IndexedFace] -> String
renderFaces fs = unlines [
  printf "POLYGONS %i %i" i (i*4)
 ,unlines $ map renderFace fs
  ]
  where
    i = length fs

renderVertex :: Vertex -> String
renderVertex (x,y,z) = unwords . map (show . round2Digit)$ [x,y,z]

renderVertices :: [Vertex] -> String
renderVertices vs = unlines [
  printf "POINTS %i float\n" (length vs)
 ,unlines $ map renderVertex vs
  ]

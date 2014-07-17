module DevSurf.Writers.OBJ (renderObj)
where
import DevSurf.Types
import DevSurf.Writers.Utils (ordElems, round2Digit)
import Data.HashMap.Lazy (size)
renderObj :: IndexedFaceSet -> String
renderObj (IndexedFaceSet fs vs ) = unlines . concat $ [
    map renderVertex $ (ordElems vs)
   ,[""]
   ,map renderFace $ (ordElems fs)
   ]

renderFace :: IndexedFace -> String
renderFace (v1,v2,v3) =
  "f " ++ (unwords . map show  $ [v1,v2,v3])

renderVertex :: Vertex -> String
renderVertex (x,y,z) = "v " ++ (unwords . map (show . round2Digit)$ [x,y,z])


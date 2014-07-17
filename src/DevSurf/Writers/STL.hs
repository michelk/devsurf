module DevSurf.Writers.STL
  ( renderSTL
  ) where
import DevSurf.Face
import DevSurf.Types

-- | Generate a STL file from a 'Mesh'.
renderSTL :: [Face] -> String
renderSTL mesh = unlines
  [ "solid "
  , concatMap face mesh
  , "endsolid "
  ]
  where
  face :: Face -> String
  face t@(a,b,c) = unlines
    [ "facet normal " ++ showVector (faceNormal t)
    , "  outer loop"
    , "    vertex " ++ showVector a
    , "    vertex " ++ showVector b
    , "    vertex " ++ showVector c
    , "  endloop"
    , "endfacet"
    ]
  showVector :: Vector -> String
  showVector (x, y, z) = show x ++ " " ++ show y ++ "  " ++ show z


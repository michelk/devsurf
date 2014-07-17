module DevSurf.Writers.Xyz where
import DevSurf.Types

-- | Vertices as space-seperated xyz-table
renderVertices :: [Vertex] -> String
renderVertices = unlines . map renderVertex

-- | Vertex space-seperated
renderVertex :: Vertex -> String
renderVertex (a,b,c) = show a ++ "   " ++ show b ++ "   " ++ show c

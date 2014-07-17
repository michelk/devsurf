module DevSurf.Readers.Xyz where
import DevSurf.Types

xyzString2Vertices :: String -> [Vertex]
xyzString2Vertices s = map xyzLine2Vertex $ lines s
  where
    xyzLine2Vertex :: String -> Vertex
    xyzLine2Vertex l = (head ws, ws !! 1, ws !! 2)
      where
        ws = map read $ words l

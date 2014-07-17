module DevSurf.Readers.ShewTriangle (parseShewTriangle)
where
import DevSurf.Types
import Data.HashMap.Lazy (fromList, HashMap)

-- Types for Shewchuck's triangle mesh generator
-- =============================================
parseShewTriangle :: 
  (
    String                      -- ^ Node string
  , String                      -- ^ Element string
  ) 
  -> IndexedFaceSet
parseShewTriangle (ns, es) =
   IndexedFaceSet (parseShewEles es) (parseShewNodes ns) 
    
parseShewNodes :: String -> HashMap Int Vertex
parseShewNodes s = fromList $ map (\i -> parseNdLine (ls !! i))[0..(nnd-1)]
  where
    ls = lines s
    hd = map read . words . head $ ls
    nnd :: Int
    nnd = head hd
    parseNdLine :: String -> (Int, Vertex)
    parseNdLine l = (read (head ws), (head coords, coords !! 1, coords !! 2))
      where
        ws = words l
        coords = map read (take 3 . tail $ ws)
    
parseShewEles :: String -> HashMap Int IndexedFace
parseShewEles = undefined

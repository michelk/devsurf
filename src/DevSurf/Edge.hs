module DevSurf.Edge where
import DevSurf.Types
import Data.List (group, sort)
       
-- | Converts a list of edges into segments by dividing, them into
-- | interior- and boundary-segments
edgesToSegments :: 
  [IndexedEdge]               -- ^ List with edges extracted from a mesh
  -> ([IndexedEdge], [IndexedEdge]) -- ^ Tuple: first is a list with
                                --   boundary edges; the second with interior ones
edgesToSegments es =  
  Prelude.foldr classifyEdge ([],[]) (group . sort . groupEdgesDir $ es)
  where
    classifyEdge :: [IndexedEdge] -> ([IndexedEdge],[IndexedEdge]) 
                    -> ([IndexedEdge],[IndexedEdge])
    classifyEdge x (bounds,segs)
      | length x == 2 = (bounds,   i: segs)
      | otherwise     = (i:bounds, segs)
      where
        i = head x

-- | Make edges direction the same, if there are two
groupEdgesDir :: [IndexedEdge] -> [IndexedEdge]
groupEdgesDir = Prelude.foldr alreadyIn [] 
  where
    alreadyIn :: IndexedEdge -> [IndexedEdge] -> [IndexedEdge]
    alreadyIn i@(i1,i2) acc 
      | (i2,i1) `elem` acc = (i2,i1):acc
      | otherwise = i:acc

-- | Forms rings from boundary segments
formEdgeRings :: 
  [IndexedEdge]                   -- ^ set of boundary edges
  -> [IndexedPolygon]             -- ^ list with polygon boundaries
formEdgeRings es = foldr groupEdges (edgeToList es) (reverse . uniqueIndices $ es)

-- | Converts a list with edges into index-polygons
edgeToList :: [IndexedEdge] -> [IndexedPolygon]
edgeToList = map (\(i1,i2) -> [i1,i2])


-- | Groups a set of polygons by chaining them according to the given index
groupEdges :: Int -> [IndexedPolygon] -> [IndexedPolygon]
groupEdges x [] = []
groupEdges x ps = ps' : rest
  where
    xs = filter f ps
    ps' 
      | length xs == 2 = merge xs
      | otherwise = head xs
    merge xss 
      | b == c = p1 ++ tail p2
      | a == c = reverse p1 ++ tail p2
      | a == d = p2 ++ tail p1
      | b == d = p1 ++ (tail . reverse $ p2)
      where
        p1 = head xss
        p2 = last xss
        a = head p1
        b = last p1
        c = head p2
        d = last p2 
    merge y =  concat y
    rest = filter (not . f) ps
    f p =  hasI x pe
      where
        pe = polygonEdges p
    hasI :: Int -> (Int,Int) -> Bool
    hasI y (i,j) 
      | i == y || j == y = True
      | otherwise = False
    polygonEdges :: IndexedPolygon -> (Int,Int)
    polygonEdges p = (head p, last p)

-- | extracts all indices of a set of edges
uniqueIndices :: [IndexedEdge] -> [Int]
uniqueIndices  = foldr alreadyIn [] 
  where
    alreadyIn (i1,i2) acc 
      | i1 `notElem` acc = i1:acc
      | i2 `notElem` acc = i2:acc
      | otherwise = acc

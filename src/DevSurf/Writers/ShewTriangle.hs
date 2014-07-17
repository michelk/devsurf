module DevSurf.Writers.ShewTriangle 
       (triangleMeshToShewPoly
       ,toShewTriangle
       ,writeShewMesh
       ,toShewPolyString 
       ,Poly
       )
where
import DevSurf.Types
import DevSurf.Edge
import DevSurf.Face
import DevSurf.Writers.Xyz (renderVertex)
import Data.HashMap.Lazy (size, toList, elems, HashMap)

type Holes = [Vertex]
type Vertices = HashMap Int Vertex
type Segments = [(Int, IndexedEdge, Int)]
type Poly = (Vertices, Segments, Holes)

-- | Converts a @IndexedFaceSet@ into a tuple of strings to be written to file
toShewTriangle :: 
  IndexedFaceSet -- ^ triangular mesh
  -> (String, String)           -- ^ tuple with node- and element-strings
toShewTriangle (IndexedFaceSet  nes nm ) = 
  (nodeMapToTriangleString nm, unlines $ esh : es)
  where
    esh = show (size nes) ++ " 3 0" -- ^ triangle-ele-header
    es = Prelude.map ele2string $ toList nes
    ele2string :: (Int, IndexedFace) -> String
    ele2string (n, (a,b,c)) = 
     show n ++ " " ++  show a ++ " " ++ show b ++ " " ++ show c

nodeMapToTriangleString :: HashMap Int Vertex -> String
nodeMapToTriangleString nm = unlines $ psh :  ns
  where
    psh = show (size nm) ++ " 2 1 0" -- ^ triangle-node-header
    ns = Prelude.map vert2string $ toList nm
    vert2string :: (Int, Vertex) -> String
    vert2string (n,(x,y,z)) = show n ++ " " ++  show x ++ " " ++ show y ++ " " ++ show z

-- | Writes a tuple of node- and element-strings to file
writeShewMesh :: 
  FilePath                      -- ^ filename 
  -> (String, String)           -- ^ node and element strings
  ->  IO ()
writeShewMesh f (ns, es)  = do
 writeFile f ns 
 writeFile f es


-- | Converts a `triangle`-.poly-tuple into a string
toShewPolyString ::  
  Poly                          -- ^ tuple with vertices, segments and holes 
  -> String                     -- ^ string to be written to file
toShewPolyString (v,s,h) = 
  nodeMapToTriangleString v ++ segmentsToTriangleString s ++ holesToTriangleString h

segmentsToTriangleString :: Segments -> String
segmentsToTriangleString ss = unlines $ sh : map seg2string ss
  where
    sh = show (length ss) ++ " 1" 
    seg2string :: (Int, IndexedEdge, Int) -> String
    seg2string (n, (i1,i2), b) = show n ++ " " ++ show i1 ++ " " ++  show i2 ++ " " ++ show b
    
holesToTriangleString :: Holes -> String
holesToTriangleString hs = unlines $ hd : hss
  where
    hd = show (length hs) 
    hss = map renderVertex hs

-- | Convert a triangular mesh to `triangle`-.poly-input
triangleMeshToShewPoly :: 
  IndexedFaceSet                  -- ^ triangular mesh
  -> Poly                       -- ^ tuple wiht vertices, segments and holes
triangleMeshToShewPoly (IndexedFaceSet em nm ) = (nm,s,h)
  where
    segs_all = concatMap indexedFaceToEdges $ elems em
    segs_div = edgesToSegments segs_all
    segs_int = snd segs_div
    segs_out =  fst segs_div
    segs_bound  = formEdgeRings segs_out
    nsint = length segs_int
    s_int = zip3 [1..] segs_int [0,0..]
    s_out = zip3 [nsint..] segs_out [1,1..]
    s = s_int ++ s_out
    h
      | length segs_bound > 1 = map (holeVertexs nm) segs_bound
      | otherwise = []

holeVertexs :: HashMap Int Vertex -> IndexedPolygon -> Vertex
holeVertexs nm  = centerFace . indexedFaceToFace nm . indexFaceFromList
                                                                               


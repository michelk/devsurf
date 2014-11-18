module DevSurf.Types where
import Data.HashMap.Lazy (HashMap)

---------------
-- Mesh Formats
-- ============

data MeshFormat = Xyz | Sms2dm | SpingNet | Obj | Ply | Stl | Dxf

--------------------------
-- FaceSet data structure
-- =======================

-- | A FaceSet represents a mesh as a list of faces
type FaceSet = [Face]

-- | A Face could be triangular, quadrilateral or polyonal
type Face  = (Vertex, Vertex, Vertex)

type Vector = (Double, Double, Double)
type Vertex = Vector
type Normal = Vector

-- | An Edge as two connected coordinates
type Edge = (Vertex, Vertex)

-- | Analogous to an OpenGL triangle strip.
type Panel = [Vertex]

-- | A curve is built up of line segments.
type Curve = [Vertex]
data PanelFace = RHR | LHR

-- Indexed face-set mesh
-- =====================
-- | Mesh as indexed face set with mapping
data IndexedFaceSet =
    IndexedFaceSet {
        ifsFaces    :: HashMap Int IndexedFace
       ,ifsVertices :: HashMap Int Vertex
    } deriving (Show)

-- | Face indexed by vertex-number
type IndexedFace  = (Int,Int,Int)

type IndexedEdge = (Int,Int)
type IndexedPolygon = [Int]

-- TODO: Half-edge data-sturcture
-- ==============================
data HalfEdgeSet = HalfEdgeSet [IndexedHalfEdge]
data IndexedHalfEdge = IndexedHalfEdge {
    iheNumber :: Int
   ,iheNd1    :: Int
   ,iheNd2    :: Int
   ,iheEleLeft :: Maybe Int
   ,iheEleRight :: Maybe Int
   ,iheType :: HalfEdgeType
}
data HalfEdgeType  = BoundaryEdge | InteriorEdge


module DevSurf.Readers.Sms2dm
       ( read2dm
       , parse2dm
       , SmsMesh
       , smsMsh
       , smsNodeStrings
       , smsMaterials
       )
where
import           Control.Applicative hiding (empty)
import           Data.Attoparsec.Text
import           Data.HashMap.Lazy (HashMap, empty, insert, adjust, keys)
import qualified Data.Text  as T
import           Data.Text.IO (readFile)
import           DevSurf.Types
import           Prelude  hiding (take, lines, takeWhile, readFile)

read2dm :: String -> IO SmsMesh
read2dm f = do
  str <- readFile f
  return $ parse2dm str

parse2dm :: T.Text -> SmsMesh
parse2dm str = case parseOnly sms2dmFile str of
    Right x -> x
    Left _  -> error "Failed parsing 2dm-file"


------------------------------------------------------------------
-- Parsers
------------------------------------------------------------------

-- | Mesh in 'IndexedFaceSet representation' with some attributes per
-- face or vertex
data SmsMesh = SmsMesh {
   smsMsh         :: IndexedFaceSet -- ^ Mesh in indexed face set representation
  ,smsNodeStrings :: HashMap Int [Int] -- ^ Nodes with boundary condition
  ,smsMaterials   :: HashMap Int [Int] -- ^ Friction-value and face-id
  ,smsNodeAttr    :: Maybe SmsNodeAttrs -- ^ Node attributes
  ,smsFaceAttr    :: Maybe SmsFaceAttrs -- ^ Face attributes
  } deriving (Show)

-- | A line of a Sms-2dm-File
data Line = FaceLine Int IndexedFace Int
          | VertexLine Int Vertex
          | NsLine Int [Int]
          | PartialNsLine [Int]
            deriving (Show)

data SmsNodeAttrs = SmsNodeAttrs {
  smsNodeAttrsName  :: String
 ,smsNodeAttrValues :: Float
 ,smsNodeAttrNodes  :: [Int]
 } deriving (Show)

data SmsFaceAttrs = SmsFaceAttrs {
  smsFaceAttrsName  :: String
 ,smsFaceAttrValues :: Float
 ,smsFaceAttrFaces  :: [Int]
 } deriving (Show)

sms2dmFile :: Parser SmsMesh
sms2dmFile = do
  ls <- lines
  let (m,_) = foldr classify (SmsMesh (IndexedFaceSet empty empty) empty empty Nothing Nothing, []) ls
  return m

classify :: Line -> (SmsMesh ,[Int]) -> (SmsMesh, [Int])
classify l (m@(SmsMesh msh@(IndexedFaceSet fcs vtxs) nss mats  _ _), pnss) =
  case l of
    FaceLine i fc vl ->
      let nfcs  = insert i fc fcs
          nmats = case vl `elem` keys mats of
            True  -> adjust ([i] ++ ) vl mats
            False -> insert vl [i] mats
      in (m {smsMsh = msh {ifsFaces = nfcs}, smsMaterials = nmats }, pnss)
    VertexLine i vtx  ->
      let nvtxs = insert i vtx vtxs
      in (m {smsMsh = msh {ifsVertices = nvtxs}}, pnss)
    PartialNsLine is  -> (m,pnss ++ is)
    NsLine i is       ->
      let nnss = insert i (pnss ++ is) nss
      in ( m {smsNodeStrings = nnss}, [])

lines :: Parser [Line]
lines = line `sepBy` endOfLine

-- | Line of Sms-2dm-file to be parsed
line :: Parser Line
line = do
  skipSpace
  t <- take 3
  skipSpace
  case T.unpack t of
    "E3T" -> triangle
    "E4Q" -> error "Quadilateral elements not supported; split them up"
    "ND " -> vertex
    "NS " -> nodeString
    _     -> skipLine *> line

triangle :: Parser Line
triangle = do
  i:v1:v2:v3:fr:_ <- indices
  return $ FaceLine i (v1,v2,v3) fr


vertex :: Parser Line
vertex = do
  skipSpace
  i <- int
  skipSpace
  x:y:z:_ <- doubles
  return $ VertexLine i (x,y,z)

nodeString :: Parser Line
nodeString = do
  skipSpace
  is <- indices
  return $ case any (<0) is of
    True  ->  NsLine (last is) (map abs $ init is)
    False ->  PartialNsLine (is)

indices :: Parser [Int]
indices = int `sepBy` (many' space)

int :: Parser Int
int = do
  r <- number
  return $ case r of
    I x -> fromInteger x
    D x -> fromEnum x

doubles :: Parser [Double]
doubles = double `sepBy` (many' space)

skipLine :: Parser ()
skipLine = skipWhile (\c -> not $ isEndOfLine c)

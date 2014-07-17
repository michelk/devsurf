module DevSurf.Readers.SpringNet
       (
         readNetFile
      -- , parseSpringNet
       )
where
import Text.ParserCombinators.Parsec
import DevSurf.Types

data SpringMesh = SpringMesh {
        springIndFaceSet :: IndexedFaceSet
       ,springAttributes :: [Attributes]
       }

-- | Read Spring-Net File
readNetFile :: FilePath -> IO Mesh
readNetFile f = undefined

type AttributeName = String
data Block = NodesBlock [(Int,VertexWithValue)]
           | ElementsBlock [(Int, IndexedFaceWithValue)]
           | AttributeBlock Attribute

data Attribute = NodeAttribute (AttributeName, [Int])
               | EleAttrbute (AttributeName, [Int])
               | NoValues AttributeName

keyword = "UNOblabiburi goes to hollywood"

main = case parse headerKeyword "(test)" keyword of
  Left err -> print err
  Right res -> print res

springNet :: GenParser Char st [Block]
springNet = many block

-- | A Block is structured in a header-line, data and a newline
--   The parser dispatches on the header-line
block :: GenParser Char st Block
block = do
  kw <- headerKeyword
  case kw of
    _ ->  undefined

headerKeyword :: GenParser Char st String
headerKeyword = many $ noneOf " \n"

-- | A Spring-mesh is a list of blocks seperated by blank lines
springNetFile :: Parser [Block]
springNetFile = undefined



-- | Parse a vertex-line
parseVertLine :: String -> [(Int,VertexWithValue)]
parseVertLine l  =
  case splitAt 26 l of
    ("","") -> []
    (x,"")  -> [parseVert x]
    (x,xs)  -> parseVert x: parseVertLine xs


-- | Parse a single vertex
parseVert :: String -> (Int,VertexWithValue)
parseVert c = (read i, ((read x,read y,0), Nothing))
  where
    (i,xy) = splitAt 6 c 
    (x,y)  = splitAt 10 xy


-- | Parse a element-line
parseEleLine :: String -> [(Int, IndexedFaceWithValue)]
parseEleLine l =
  case splitAt 36 l  of
    ("","") -> []
    (x, "") -> [parseEle x]
    (x,xs) -> parseEle x: parseEleLine xs

-- | Parse a single element
parseEle :: String -> (Int, IndexedFaceWithValue)
parseEle s =  
  case v4 of
    0 -> (i, ((v1,v2,v3) [], Nothing))
  where
    i:v1:v2:v3:v4:[] = take 5 $ parseEleItem s

parseEleItem :: String -> [Int]
parseEleItem s = 
  case splitAt 6 s of
    ("","") -> []
    (x, "") -> [read x]
    (x,xs) -> read x : parseEleItem xs

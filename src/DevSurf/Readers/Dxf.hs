{-# LANGUAGE OverloadedStrings #-}
module DevSurf.Readers.Dxf (parseDxf, readDxf) where

import Control.Applicative
import Data.Attoparsec.Text
import DevSurf.Readers.Utils
import Data.Text  as T
import Data.Text.IO  as TIO
import DevSurf.Types

readDxf :: FilePath -> IO FaceSet
readDxf f =
  do str <- TIO.readFile f
     return $ parseDxf str

parseDxf :: T.Text -> FaceSet
parseDxf str =
  case parseOnly dxf str of
    Right x -> x
    Left _ -> error "Failed parsing dxf-file"

dxf :: Parser FaceSet
dxf =
  do skipHeader
     fcs <- many' face
     skipFooter
     return fcs

skipHeader :: Parser ()
skipHeader = skipId *> "SECTION" *> endOfLine *> skipId *> "ENTITIES" *>
             endOfLine *> skipId

skipFooter :: Parser ()
skipFooter =
  do "ENDSEC" *> endOfLine
     skipId
     "EOF" *> endOfLine

face :: Parser Face
face =
  do "3DFACE" *> endOfLine *> skipId *> skipId
     v1 <- vertex
     v2 <- vertex
     v3 <- vertex
     _ <- vertex
     skipId
     return (v1,v2,v3)

vertex :: Parser Vertex
vertex =
  do x <- coordComp
     y <- coordComp
     z <- coordComp
     return (x,y,z)

coordComp :: Parser Double
coordComp = skipId *> float <* endOfLine

skipId :: Parser ()
skipId = skipSpace *> int *> endOfLine

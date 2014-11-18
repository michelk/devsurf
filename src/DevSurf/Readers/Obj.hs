{-# LANGUAGE OverloadedStrings #-}
module DevSurf.Readers.Obj (parseObj, readObj) where
import DevSurf.Readers.Utils
import Data.Attoparsec.Text
import Data.Text  as T
import Data.Text.IO  as TIO
import DevSurf.Types
import Data.HashMap.Lazy
import Data.Char (isSpace)

readObj :: FilePath -> IO IndexedFaceSet
readObj f =
  do str <- TIO.readFile f
     return $ parseObj str

parseObj :: T.Text -> IndexedFaceSet
parseObj str =
  case parseOnly obj str of
    Right x -> x
    Left _ -> error "Failed parsing obj-file"

obj :: Parser IndexedFaceSet
obj =
  do skipLWS
     nds <- vertices
     skipLWS
     fcs <- faces
     skipLWS
     endOfInput
     return $
       IndexedFaceSet fcs nds

vertex :: Parser Vertex
vertex =
  do "v"
     skipSpace
     x <- float
     skipSpace
     y <- float
     skipSpace
     z <- float
     endOfLine
     return $
       (x,y,z)

vertices :: Parser (HashMap Int Vertex)
vertices =
  do nds <- many' vertex
     return $
       fromList $
       zip [1 ..] nds

face :: Parser IndexedFace
face =
  do "f"
     skipSpace
     v1 <- int
     skipSpace
     v2 <- int
     skipSpace
     v3 <- int
     endOfLine
     return $
       (v1,v2,v3)

faces :: Parser (HashMap Int IndexedFace)
faces =
  do fcs <- many' face
     return $
       fromList $
       zip [1 ..] fcs

data Skip = Space | Comment

-- | Skip lines, comments, or horizontal white space.
skipLWS :: Parser ()
skipLWS =
  scan Space go *>
  pure ()
  where go Space c
          | isSpace c = Just Space
        go Space '#' = Just Comment
        go Space _ = Nothing
        go Comment '\r' = Just Space
        go Comment '\n' = Just Space
        go Comment _ = Just Comment

-- | Skip comments or horizontal white space.
skipHWS :: Parser ()
skipHWS =
  scan Space go *>
  pure ()
  where go Space ' ' = Just Space
        go Space '\t' = Just Space
        go Space '#' = Just Comment
        go Space _ = Nothing
        go Comment '\r' = Nothing
        go Comment '\n' = Nothing
        go Comment _ = Just Comment


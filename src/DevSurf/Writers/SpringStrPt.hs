module DevSurf.Writers.SpringStrPt
       (renderStrPt
       ,renderDefautlStrPt
       )
where
import DevSurf.Types
import Text.Printf (printf)

data StrType = Point  |  Line | Polygon
data StrConfig = StrConfig {
  strType   :: StrType
 ,strColor  :: Int
 ,strDesc   :: String
 ,strAttr   :: String
 ,strSymbol :: Int
 ,strSize   :: Int
  }
defaultConfig :: StrConfig
defaultConfig = StrConfig {
   strType   = Point
  ,strColor  = 53               --  red
  ,strDesc   = "__"
  ,strAttr   = "KKKK"
  ,strSymbol = 3
  ,strSize   = 3
  }
renderStrPt :: StrConfig -> [Vertex] -> String
renderStrPt cfg vs = unlines [
  renderHeader cfg
 ,concatMap renderVertex vs
  ]

renderDefautlStrPt :: [Vertex] -> String
renderDefautlStrPt = renderStrPt defaultConfig

renderVertex :: Vertex -> String
renderVertex (x,y,z) = printf "%6s%10.2f%10.2f%7.3f\n" ("" :: String) x y z

renderHeader :: StrConfig -> String
renderHeader (StrConfig t c d a s g) =
  printf "\n\n\n\n%s | %s | @ %i %i %i @ %s" a (ts :: String) c s g d
  where
    ts = case t of
      Point   -> "p"
      Line    -> "l"
      Polygon -> "f"

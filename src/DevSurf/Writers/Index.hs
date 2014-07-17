module DevSurf.Writers.Index where
import DevSurf.Types

-- | Index in a line
renderIndices :: [Int] -> String
renderIndices = unwords . map renderIndex

-- | Index
renderIndex :: Int -> String
renderIndex i = show i

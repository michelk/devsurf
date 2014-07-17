module Main where
import System.Environment
import Text.Regex.Posix
import System.Cmd
import System.FilePath.Posix (replaceExtension)
import Control.Monad
import DevSurf.Types
import DevSurf.Readers.Sms2dm
import DevSurf.Readers.Xyz
import DevSurf.Writers.ShewTriangle
import Data.HashMap.Lazy (fromList, union, size)
import System.FilePath.Posix (replaceExtension)


main :: IO ()
main = do
  ext_tr <- system "triangle -h > /dev/null"
  when ((show ext_tr) /=  "ExitSuccess") (fail "Shewcuk's triangle mesh-generator is not installed")
  args <- getArgs
  -- Read mesh and points
  let pf = head args          --  ^ xyz-point-file
      mf = head . tail $ args --  ^ mesh-file
  ps <- readFile pf           --  ^ xyz-string
  ms <- readFile mf           --  ^ mesh-string
  let m = parse2dm ms
      pp = xyzString2Vertices ps
  -- Create Triangle-poly-file
      poly = injectPointsMesh pp m
      pfn = replaceExtension mf "poly"
  writeFile pfn poly

  -- Run Triangle
  ext_tr2 <- system (triangle ++ " -pQ " ++ pfn)
  when  ((show ext_tr2) /=  "ExitSuccess") (fail "Shewcuk's triangle mesh-generator is failed")
  
  -- Read mesh in
  -- Convert to 2dm-format
  putStrLn ps
  
triangle :: String
triangle = "triangle"

injectPointsMesh :: [Vertex]-> SmsMesh -> String
injectPointsMesh pp m = toShewPolyString poly'
  where
    poly = triangleMeshToShewPoly . smsMsh $ m
    poly' = addPointsPoly poly pp
    addPointsPoly :: Poly -> [Vertex] -> Poly
    addPointsPoly (vert,s,h) ps = (vert',s,h)
      where
        vertP = fromList $ zip [((size vert ) + 1)..] ps
        vert' = union vert vertP
    
    

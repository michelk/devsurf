-- | This program scans the elements of sms-2dm-file and omits the
--   smallest elements and those with highest aspect-ratio
module Main where
import DevSurf
import Data.HashMap.Lazy (toList)
import qualified Data.HashMap.Lazy as M
import Text.Printf (printf)
import Data.Function (on)
import Data.List (sortBy)

main :: IO ()
main = interact $ render . checkFaces . smsMsh . parse2dm

type FaceValues = 
  (
   Double     -- ^ Area
  ,Double     -- ^ Aspect-Ratio
  ,Double     -- ^ Product of Inverse Area and Aspect-Ratio
  )
-- | Number of elements to omit
_N :: Int
_N = 10

-- | Analysis Faces of a mesh regarding area and aspect-ratio
checkFaces :: IndexedFaceSet -> [(Int,FaceValues)]
checkFaces (IndexedFaceSet em nm ) =
  toList . M.map (checkFace . indexedFaceToFace nm) $  em

-- | Analysis a single face regarding area and aspect-ratio
checkFace :: Face -> FaceValues
checkFace x = (a, r, (1/a)*r)
  where
    a = area x
    r = aspectRatioFace x

-- | Renders the calculated face-values
render :: [(Int,FaceValues)] -> String
render xs = 
  concat
  [
    renderList "smallest" "Area" id fst' xs
   ,renderList "largest" "Aspect-Ratio" reverse snd' xs
   ,renderList "largest" "1/Area x Apspect-Ratio" reverse thrd xs
  ]
  where
    renderList adj noun rFun eFun ls =
      renderHeader adj noun ++
      (concatMap (renderFaceValues noun) . take _N . rFun $ sortBy (compare `on` eFun)  ls)
    fst' (_,(x,_,_)) = x
    snd' (_,(_,x,_)) = x
    thrd (_,(_,_,x)) = x

renderHeader :: String -> String -> String
renderHeader t s = "* Ten " ++ t ++ " " ++ s ++ " elements:\n" ++
                 printf "\t%6s  %10s\n" "Element" s

renderFaceValues :: String -> (Int,FaceValues) -> String
renderFaceValues what (i,(a,r,ar)) = printf "\t%6i  %10.3f\n" i v
  where
    v = case what of
      "Area"                   -> a
      "Aspect-Ratio"           -> r
      "1/Area x Apspect-Ratio" -> ar

module DevSurf.Writers.Utils where
import Data.HashMap.Lazy (HashMap, keys, lookup)
import Data.List (sort)
import Prelude  hiding (lookup)
import Data.Maybe (fromJust)

ordElems :: HashMap Int a -> [a]
ordElems m = map (\k -> fromJust $ lookup k m) (sort . keys $ m)

toOrdList :: HashMap Int a -> [(Int,a)]
toOrdList m = zip ks vs
    where
        ks = sort. keys $ m
        vs = map (\k -> fromJust $ lookup k m) ks

round2Digit :: Double -> Double
round2Digit x = fromIntegral (round (x * 100)) / 100

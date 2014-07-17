module DevSurf.PanelFace
where
import DevSurf.Types
import Data.List (zip4)
-- | Generate a 'Mesh' from a 'Panel'.  XXX Need to handle different face directions.
meshPanel :: PanelFace -> Panel -> [Face]
meshPanel face a = case a of
  _ : _ : _ : _ -> 
    [ if flip then (b,a,c) else (a,b,c) | (flip, a, b, c) <- zip4 flips a (tail a) (tail $ tail a) ]
  _             -> error "meshPanel: Not enough points to form a triangle."
  where
  flips = concat $ repeat $ case face of
    RHR -> [False, True]
    LHR -> [True, False]



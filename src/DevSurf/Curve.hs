module DevSurf.Curve
where
import DevSurf.Types
import DevSurf.Vector (magnitude, sub)
import Data.List (find)
import Data.Maybe (fromJust)

-- | Subdivide a 'Curve' once.
subdivide :: Curve -> Curve
subdivide = f2 . f1
  where
  f1 :: Curve -> Curve
  f1 a = case a of
    a : b : c -> a : ave2 a b : f1 (b : c)
    a -> a

  f2 :: Curve -> Curve
  f2 a = case a of
    [] -> []
    a : b -> a : f3 b

  f3 :: Curve -> Curve
  f3 a = case a of
    a : b : c : d -> a : ave3 a b c : f3 (c : d)
    a -> a

  ave2 :: Vertex -> Vertex -> Vertex
  ave2 (x1, y1, z1) (x2, y2, z2) = 
    ((x1 + x2) / 2, (y1 + y2) / 2, (z1 + z2) / 2)

  ave3 :: Vertex -> Vertex -> Vertex -> Vertex
  ave3 (x1, y1, z1) (x2, y2, z2) (x3, y3, z3) = 
    ((x1 + x2 + x3) / 3, (y1 + y2 + y3) / 3, (z1 + z2 + z3) / 3)

-- | Subdivde a 'Curve' N times.
subdivideN :: Int -> Curve -> Curve
subdivideN n a = iterate subdivide a !! n

-- | Subdivide a 'Curve' until max distance between points has a given precision.
subdivideToPrecision :: Double -> Curve -> Curve
subdivideToPrecision p a = fromJust $ find prec $ iterate subdivide a
  where
  prec :: Curve -> Bool
  prec a = all (<= p) [ magnitude (b `sub` a) | (a, b) <- zip a (tail' a) ]

  tail' a = case a of
    [] -> []
    _ : a -> a

-- | Loft a 'Panel' between two 'Curve's.
loft :: Curve -> Curve -> Panel
loft a b = concat [ [a, b] | (a, b) <- zip a b ]

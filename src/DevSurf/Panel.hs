module DevSurf.Panel
where
import DevSurf.Types
import DevSurf.Vector
import DevSurf.Face (faceNormal)
import DevSurf.Transform
import DevSurf.PanelFace (meshPanel)

       
-- | Given a surface normal dot product threshold, checks if a panel can be flattened (unrolled).
canFlatten :: Double -> Panel -> Bool
canFlatten threshold panel = all (>= threshold) [ a `dot` b | (a, b) <- zip normals $ tail normals ]
  where
  normals = map faceNormal $ meshPanel RHR panel

-- | Flatten (unroll) a 'Panel' to the XY plane.
flatten :: Panel -> Panel
flatten (a : rest) = f [(0, 0, 0)] $ move (neg a) rest
  where
  -- On entry: last point at origin, second to last point on Y axis.
  f :: Panel -> Panel -> Panel
  f sofar [] = sofar
  f b0 a0@((x0, _, z0) : _) = f (b4 ++ [p4]) a4
    where
    -- Rotate point to X-Y plane.
    a1@((x1, y1, _) : _) = rotateY (atan2 z0 x0) a0
    -- Rotate point to positive Y axis.
    a2@(p2 : _) = rotateZ (pi / 2 - atan2 y1 x1) a1
    b2          = rotateZ (pi / 2 - atan2 y1 x1) b0
    -- Move point to origin.
    a3 = move (neg p2) a2
    b3 = move (neg p2) b2
    -- Scale to mirror X-Z plane.
    p4 : a4 = scale (1, -1, 1) a3
    b4      = scale (1, -1, 1) b3

flatten _ = error "flatten: Not enough points to form a triangle."

-- | Build a profile of a 'Panel'
profile :: Panel -> Curve
profile curve = a ++ reverse b ++ [head a]
  where
  (a, b) = split curve
  split :: [Vector] -> ([Vector], [Vector])
  split a = case a of
    [] -> ([], [])
    [a] -> ([a], [])
    a : b : rest -> (a : a', b : b') where (a', b') = split rest


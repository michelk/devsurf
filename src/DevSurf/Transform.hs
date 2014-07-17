{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module DevSurf.Transform where
import DevSurf.Types
import DevSurf.Vector (add)


class    Transform a                  where transform :: (Vector -> Vector) -> a -> a
instance Transform Vector             where transform f  = f 
instance Transform Face               where transform f (a,b,c) = ((f a),(f b),(f c))
instance Transform a => Transform [a] where transform f  = map (transform f) 

move :: Transform a => Vector -> a -> a
move a = transform $ add a

scale :: Transform a => Vector -> a -> a
scale (a, b, c) = transform $ \(x, y, z) -> (a * x, b * y, c * z)

rotateX :: Transform a => Double -> a -> a
rotateX angle = transform f
  where
  f (x, y, z) = (x, m * cos angle', m * sin angle')
    where
    angle' = angle + atan2 z y
    m = sqrt $ y ** 2 + z ** 2

rotateY :: Transform a => Double -> a -> a
rotateY angle = transform f
  where
  f (x, y, z) = ((-m) * cos angle', y, m * sin angle')
    where
    angle' = angle + atan2 z (-x)
    m = sqrt $ x ** 2 + z ** 2

rotateZ :: Transform a => Double -> a -> a
rotateZ angle = transform f
  where
  f (x, y, z) = (m * cos angle', m * sin angle', z)
    where
    angle' = angle + atan2 y x
    m = sqrt $ x ** 2 + y ** 2

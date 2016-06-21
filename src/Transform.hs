
module Transform where

data Coord a = Coord { x :: a, y :: a, z :: a, u :: a } deriving (Show)


type RotationMatrix a =
 ( (a, a, a, a),
   (a, a, a, a),
   (a, a, a, a),
   (a, a, a, a) )

transformCoord :: (Num a) => RotationMatrix a -> Coord a -> Coord a
transformCoord
  ( (a11, a12, a13, a14),
    (a21, a22, a23, a24),
    (a31, a32, a33, a34),
    (a41, a42, a43, a44) )
  (Coord x y z u) =
  Coord
  (a11*x + a12*y + a13*z + a14*u)
  (a21*x + a22*y + a23*z + a24*u)
  (a31*x + a32*y + a33*z + a34*u)
  (a41*x + a42*y + a43*z + a44*u )

rotationX :: Double -> RotationMatrix Double
rotationX th =
  ( (1, 0,      0,       0),
    (0, cos th, -sin th, 0),
    (0, sin th, cos th,  0),
    (0, 0,      0,       1) )

rotationY :: Double -> RotationMatrix Double
rotationY th =
  ( (cos th,  0, sin th, 0),
    (0,       1, 0     , 0),
    (-sin th, 0, cos th, 0),
    (0,       0, 0,      1) )

rotationZ :: Double -> RotationMatrix Double
rotationZ th =
  ( (cos th, -sin th, 0, 0),
    (sin th, cos th,  0, 0),
    (0,      0,       1, 0),
    (0,      0,       0, 1) )

mapCoord :: (Coord a -> Coord a) -> (Coord a, b) -> (Coord a, b)
mapCoord f (c, x) = (f c, x)

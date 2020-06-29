module Lib
    ( V2,
      origin,
      east,
      Ray ( Ray ),
      rayAdd
    ) where

data V2 = V2 Float Float deriving (Show, Eq)
origin = V2 0.0 0.0
east = V2 1.0 0.0

type Radians = Float
data Ray = Ray { relPos :: V2, radians :: Radians }

rayAdd :: Ray -> Ray -> Ray
rayAdd (Ray (V2 x1 y1) θ) (Ray (V2 x2 y2) φ) =
    (Ray (V2 (x1 + x2Rotated) (y1 + y2Rotated)) (θ + φ))
  where
   x2Rotated = x2 * cos(θ) - y2 * sin(θ)
   y2Rotated = x2 * sin(θ) + y2 * cos(θ) 

module Main where

import Lib (V2, Ray ( Ray ), rayAdd, origin, east)

data JoinerKind = In | Out deriving Show
data Piece = Straight { length :: Float,
                        start :: JoinerKind,
                        end :: JoinerKind }
           | Curve { arcLength :: Float,
                     radius :: Float,
                     start :: JoinerKind,
                     end :: JoinerKind }
    deriving (Show)

-- General assumption is that pieces default orientation is to the east = (1, 0).

-- connectors :: Ray -> Piece -> [Connector]
connectors ray (Straight length start end) =
  [(rayAdd ray (Ray origin pi), start),
   (rayAdd ray (Ray east 0.0), end)]
-- connectors _ _ = unknown

straight = Straight 6.0 In Out
joinerIn = Straight 2.0 In In
joinerOut = Straight 2.0 Out Out

arc radius count = Curve arcLength radius In Out
  where
    arcLength = (pi * 4.0 * radius) / count

curveLong = arc 10.0 8
curveShort = arc 7.5 8

pieces :: [Piece]
pieces = (replicate 10 straight)
         ++ (replicate 20 curveLong)
         ++ (replicate 10 curveShort)
         ++ (replicate 2 joinerIn)
         ++ (replicate 2 joinerOut)

main :: IO ()
main = do
  print pieces

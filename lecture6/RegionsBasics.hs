module RegionsBasics where

import Prelude hiding (empty)

data Point = Point Float Float
  deriving Show
type Region = Point -> Bool

empty :: Region
empty = const False

whole :: Region
whole = const True

hits :: Region -> Point -> Bool
hits region = region

p1 :: Point
p1 = Point 0 0

p2 :: Point
p2 = Point 1 1

r1 :: Region
r1 (Point 1 1) = True
r1 _ = False

module RegionsConstruction where

import RegionsBasics

p1 :: Point
p1 = Point 0 0
p2 :: Point
p2 = Point 6 6
r1 :: Region
r1 = rectangle 5 5
r2 :: Region
r2 = rectangle 10 10
rs :: [Region]
rs = [r1, r2]

circle :: Float -> Region
circle r = \(Point x y) ->
  x <=  r &&
  y <=  r &&
  x >= -r &&
  y >= -r

half :: Float -> Float
half n = n / 2

rectangle :: Float -> Float -> Region
rectangle w h = \(Point x y) ->
  x <=  (half w) &&
  x >= -(half w) &&
  y <=  (half h) &&
  y >= -(half y)

outside :: Region -> Region
-- outside r = \p -> not (r p)
outside r = not . r

(/\) :: Region -> Region -> Region
r1 /\ r2 = \p -> r1 p && r2 p

(\/) :: Region -> Region -> Region
r1 \/ r2 = \p -> r1 p || r2 p

intersect :: [Region] -> Region
intersect = foldr (/\) (\p -> True)

union :: [Region] -> Region
union = foldr (\/) (\p -> False)

translate :: Region -> Float -> Float -> Region
translate r x' y' = \p -> r

ring :: Float -> Float -> Region
ring cr1 cr2 = outside (circle cr1) /\ circle cr2

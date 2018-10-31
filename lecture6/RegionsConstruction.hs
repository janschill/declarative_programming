module RegionsConstruction where

import RegionsBasics

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
translate r x' y' = \(Point x y) -> r (Point (x-x') (y-y'))

ring :: Float -> Float -> Region
ring cr1 cr2 = outside (circle cr1) /\ circle cr2

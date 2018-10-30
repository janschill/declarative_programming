module RegionsConstruction where

import RegionsBasics

circle :: Float -> Region
circle r =
  \p -> if p (Point 0 0) then True else False
circle _ = empty

-- rectangle :: Float -> Float -> Region

-- outside :: Region -> Region

-- intersect :: [Region] -> Region
-- (/\) :: Region -> Region -> Region

-- union :: [Region] -> Region
-- (\/) :: Region -> Region -> Region

-- translate :: Region -> Float -> Float -> Region

-- ring :: Float -> Float -> Region
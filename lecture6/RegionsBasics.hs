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

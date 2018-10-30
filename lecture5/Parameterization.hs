module Parameterization where

import Prelude hiding (id, const, flip, IO, length, compare)

id :: a -> a
id x = x
-- id = \x -> x

const :: a -> b -> a
const x _ = x
-- const x = \y -> x
-- const = \x -> \y -> x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = f (g x) (g y)

-- Examples
-- id
countTrues :: [Bool] -> Int
countTrues bs = length (filter id bs)

countFalses :: [Bool] -> Int
countFalses bs = length (filter not bs)

-- const
length :: [a] -> Int
length = foldr (const (1+)) 0

sameLength :: [Double] -> [Int]
sameLength = map (const 23)

extend :: [Double] -> [(Double, Int)]
extend ds = zip ds (sameLength ds)

-- flip
divideInverse :: Int -> Int -> Int
divideInverse = flip div

powers :: [Int] -> [Int]
powers xs = map ((^) 2) xs

squares :: [Int] -> [Int]
squares xs = map (flip (^) 2) xs

-- on
data IO = I | O
  deriving Show

ioToInt :: IO -> Int
ioToInt O = 0
ioToInt I = 1

compare :: IO -> IO -> Bool
compare = on (==) ioToInt

module Parameterization where

import Prelude hiding (id, const, flip, IO)

id :: a -> a
id x = x
-- id = \x -> x

const :: a -> b -> a
const x y = x
-- const x = \y -> x
-- const = \x -> \y -> x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = f (g x) (g y)

-- Examples

length' :: [a] -> Int
length' = foldr (const (1+)) 0

data IO = I | O
  deriving Show

ioToInt :: IO -> Int
ioToInt O = 0
ioToInt I = 1

sumToString :: Int -> Int -> String
sumToString x y = show (x + y)

divideInverse :: Int -> Int -> Int
divideInverse x y = flip div x y

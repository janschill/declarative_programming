module List where

takeList :: Int -> [a] -> [a]
takeList 0 list = []
takeList n (x:xs) = x : takeList (n-1) xs

appendList :: [a] -> [a] -> [a]
appendList [] list = list
appendList list [] = list
appendList (x:xs) list = x : (appendList xs list)

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = appendList (reverseList xs) [x]

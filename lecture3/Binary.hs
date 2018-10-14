module Binary where

data BinTree a = Empty | Node (BinTree a) a (BinTree a)
  deriving Show

tree = Node (Node Empty 3 Empty) 5 (Node Empty 7 (Node Empty 8 (Node Empty 13 Empty)))

sumTree :: BinTree Integer -> Integer
sumTree Empty = 0
sumTree (Node bt1 num bt2) = sumTree bt1 + num + sumTree bt2

values :: BinTree a -> [a]
values Empty = []
values (Node bt1 value bt2) = values bt1 ++ [value] ++ values bt2

mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree fun Empty = Empty
mapTree fun (Node bt1 value bt2) = Node (mapTree fun bt1) (fun value) (mapTree fun bt2)

levels :: BinTree a -> [[a]]
levels Empty = [[]]
levels (Node bt1 value bt2) = [[value], concat (levels bt1 ++ levels bt2)]

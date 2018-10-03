module List where

data List a = Nil | Cons a (List a)
  deriving Show

defaultIntList = Cons 1 (Cons 2 Nil)
defaultIntList2 = Cons 3 (Cons 2 (Cons 4 Nil))
defaultCharList = Cons 'a' (Cons 'b' Nil)

takeList :: Int -> (List a) -> (List a)
takeList 0 _ = Nil
takeList n (Cons x xs) = Cons x (takeList (n-1) xs)

appendList :: (List a) -> (List a) -> (List a)
appendList list Nil = list
appendList Nil list = list
appendList (Cons n Nil) list = Cons n list
appendList (Cons n ns) list = Cons n (appendList ns list)

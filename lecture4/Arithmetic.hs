module Arithmetic where

data BinOp = Add | Sub | Mul | Div
data Expr
  = BinApp BinOp Expr Expr
  | Num Int

expr1 = BinApp Add (BinApp Mul (Num 2) (Num 3)) (Num 4)

showExpr :: Expr -> String
showExpr (Num num) = show num
showExpr (BinApp binOp expr1 expr2) = "(" ++ showExpr expr1 ++ operation binOp ++ showExpr expr2 ++ ")"
  where
    operation :: BinOp -> String
    operation Add = " + "
    operation Sub = " - "
    operation Mul = " * "
    operation Div = " / "

safeAdd :: Int -> Int -> Maybe Int
safeAdd x y = Just (x + y)

safeSub :: Int -> Int -> Maybe Int
safeSub x y = Just (x - y)

safeMul :: Int -> Int -> Maybe Int
safeMul x y = Just (x * y)

safeDiv :: Int -> Int -> Maybe Int
safeDiv x 0 = Nothing
safeDiv x y = Just (x `div` y)

andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen (Just x) f = f x

eval :: Expr -> Maybe Int
eval (Num num) = Just num
eval binApp =
  case binApp of
    BinApp Add expr1 expr2 -> eval' expr1 expr2 safeAdd
    BinApp Sub expr1 expr2 -> eval' expr1 expr2 safeSub
    BinApp Mul expr1 expr2 -> eval' expr1 expr2 safeMul
    BinApp Div expr1 expr2 -> eval' expr1 expr2 safeDiv
    where
      eval' :: Expr -> Expr -> (Int -> Int -> Maybe b) -> Maybe b
      eval' (Num num) expr f = eval expr `andThen` f num
      eval' expr (Num num) f = eval expr `andThen` f num

-- map (maybeMap (+1)) [Just 5,Just 1] -> [Just 6,Just 2]
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just (f x)
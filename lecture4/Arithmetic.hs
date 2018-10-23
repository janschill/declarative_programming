module Arithmetic where

data BinOp = Add | Sub | Mul | Div
data Expr
  = BinApp BinOp Expr Expr
  | Num Int

expr1 :: Expr
expr1 = BinApp Add (BinApp Mul (Num 2) (Num 3)) (Num 4)

showExpr :: Expr -> String
showExpr (Num num) = show num
showExpr (BinApp binOp expr1 expr2) =
  "(" ++ showExpr expr1 ++ operator binOp ++ showExpr expr2 ++ ")"
    where
      operator :: BinOp -> String
      operator Add = " + "
      operator Sub = " - "
      operator Mul = " * "
      operator Div = " / "

eval :: Expr -> Int
eval (Num n) = n
eval (BinApp op e1 e2) = applyOp op (eval e1) (eval e2)

applyOp :: BinOp -> Int -> Int -> Int
applyOp Add x y = x + y
applyOp Sub x y = x - y
applyOp Mul x y = x * y
applyOp Div x y = div x y

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
andThen Nothing _ = Nothing
andThen (Just x) f = f x

safeEval' :: Expr -> Maybe Int
safeEval' (Num num) = Just num
safeEval' binApp =
  case binApp of
    BinApp Add expr1 expr2 -> eval_ expr1 expr2 safeAdd
    BinApp Sub expr1 expr2 -> eval_ expr1 expr2 safeSub
    BinApp Mul expr1 expr2 -> eval_ expr1 expr2 safeMul
    BinApp Div expr1 expr2 -> eval_ expr1 expr2 safeDiv
    where
      eval_ :: Expr -> Expr -> (Int -> Int -> Maybe b) -> Maybe b
      eval_ (Num num) expr f = safeEval' expr `andThen` f num
      eval_ expr (Num num) f = safeEval' expr `andThen` f num

safeEval :: Expr -> Maybe Int
safeEval (Num n) = Just n
safeEval (BinApp Div e1 e2) =
  case (safeEval e2) of
    Nothing -> Nothing
    Just 0  -> Nothing
    Just x1 -> case (safeEval e1) of
      Nothing -> Nothing
      Just x2 -> Just (applyOp Div x1 x2)
safeEval (BinApp op e1 e2) =
  case (safeEval e1) of
    Nothing -> Nothing
    Just x1 -> case (safeEval e2) of
      Nothing -> Nothing
      Just x2 -> Just (applyOp op x1 x2)

safeEval'' :: Expr -> Maybe Int
safeEval'' (Num n) = Just n
safeEval'' (BinApp op e1 e2) =
  safeEval'' e1 `andThen` (\x1 ->
  safeEval'' e2 `andThen` (\x2 ->
  Just (applyOp op x1 x2)))

-- map (maybeMap (+1)) [Just 5,Just 1] -> [Just 6,Just 2]
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just (f x)
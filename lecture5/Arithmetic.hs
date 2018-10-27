module Arithmetic where

data BinOp = Add | Sub | Mul | Div
data Expr
  = BinApp BinOp Expr Expr
  | Num Int
type Error = [String]

expr1 :: Expr
expr1 = BinApp Add (BinApp Mul (Num 2) (Num 3)) (Num 4)

expr2 :: Expr
expr2 = BinApp Add (BinApp Div (Num 2) (Num 0)) (Num 4)

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

andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing _ = Nothing
andThen (Just x) f = f x

safeEval' :: Expr -> Maybe Int
safeEval' (Num n) = Just n
safeEval' (BinApp op e1 e2) =
  safeEval' e1 `andThen` (\x1 ->
  safeEval' e2 `andThen` (\x2 ->
    case op of
      Div -> case x2 of
          0 -> Nothing
          _ -> Just (applyOp Div x1 x2)
      _ -> Just (applyOp op x1 x2)))

eval1 :: Expr -> Either Error Int
eval1 (Num n) = Right (n)
eval1 (BinApp Div e1 e2) =
  case (eval1 e2) of
    Left error -> Left error
    Right 0 -> Left ["Exception: divide by zero"]
    Right x1 -> case (eval1 e1) of
      Left error -> Left error
      Right x2 -> Right (applyOp Div x1 x2)
eval1 (BinApp op e1 e2) =
  case (eval1 e1) of
  Left error -> Left error
  Right x1 -> case (eval1 e2) of
    Left error -> Left error
    Right x2 -> Right (applyOp op x1 x2)

andThen' :: Either c a -> (a -> Either c b) -> Either c b
andThen' (Left x) f = Left x
andThen' (Right x) f = f x

eval2 :: Expr -> Either Error Int
eval2 (Num n) = Right (n)
eval2 (BinApp op e1 e2) =
  eval2 e1 `andThen'` (\x1 ->
  eval2 e2 `andThen'` (\x2 ->
    case op of
      Div -> case x2 of
          0 -> Left ["Exception: divide by zero"]
          _ -> Right (applyOp Div x1 x2)
      _ -> Right (applyOp op x1 x2)))

-- map (maybeMap (+1)) [Just 5,Just 1] -> [Just 6,Just 2]
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just (f x)

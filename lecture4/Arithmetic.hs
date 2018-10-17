data BinOp = Add | Sub | Mul | Div
data Expr
  = BinApp BinOp Expr Expr
  | Num Int

expr1 = BinApp Add (BinApp Mul (Num 2) (Num 3)) (Num 4)

showExpr :: Expr -> String
showExpr (Num num) = show num
showExpr binApp =
  case binApp of
    BinApp Add expr1 expr2 -> "(" ++ showExpr expr1 ++ " + " ++ showExpr expr2 ++ ")"
    BinApp Sub expr1 expr2 -> "(" ++ showExpr expr1 ++ " - " ++ showExpr expr2 ++ ")"
    BinApp Mul expr1 expr2 -> "(" ++ showExpr expr1 ++ " * " ++ showExpr expr2 ++ ")"
    BinApp Div expr1 expr2 -> "(" ++ showExpr expr1 ++ " / " ++ showExpr expr2 ++ ")"

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
andThen (Just x) fun = fun x

eval :: Expr -> Maybe Int
eval (Num num) = Just num
eval binApp =
  case binApp of
    BinApp Add (Num num) expr -> eval expr `andThen` safeAdd num
    BinApp Add expr (Num num) -> eval expr `andThen` safeAdd num
    BinApp Sub (Num num) expr -> eval expr `andThen` safeSub num
    BinApp Sub expr (Num num) -> eval expr `andThen` safeSub num
    BinApp Mul (Num num) expr -> eval expr `andThen` safeMul num
    BinApp Mul expr (Num num) -> eval expr `andThen` safeMul num
    BinApp Div (Num num) expr -> eval expr `andThen` safeDiv num
    BinApp Div expr (Num num) -> eval expr `andThen` safeDiv num

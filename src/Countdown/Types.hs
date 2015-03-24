module Types where


data Expr = Num Int | App Op Expr Expr

data Op = Add | Sub | Mul | Div deriving (Eq)

type Value = Int

instance Show Expr where
  show (Num n) = show n
  show (App op e1 e2) = " ( " ++ show e1 ++ show op ++ show e2 ++ " ) "

instance Show Op where
  show Add = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " / "


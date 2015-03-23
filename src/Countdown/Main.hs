module Main where

import Data.List
import Data.Function
import System.Environment (getArgs)

sources :: [Int]
sources = [1,3,7,10,25,50]

target :: Int
target = 831

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

main :: IO ()
main = do
  [s, ss] <- getArgs
  let n = read s :: Int
      list = read ss :: [Int]
      (e, v) = countdown1 n list
  putStrLn $ (show e) ++ " = " ++ show v

countdown1 :: Int -> [Int] -> (Expr, Value)
countdown1 n = nearest n . concatMap mkExprs . subseqs

subseqs :: [Value] -> [[Value]]
subseqs [x] = [[x]]
subseqs (x:xs) = xss ++ [x] : map (x:) xss
  where xss = subseqs xs

value :: Expr -> Value
value (Num n) = n
value (App op e1 e2) = apply op (value e1) (value e2)

apply :: Op -> Value -> Value -> Value
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = div

legal :: Op -> Value -> Value -> Bool
legal Add v1 v2 = True
legal Sub v1 v2 = v2 < v1
legal Mul v1 v2 = True
legal Div v1 v2 = v1 `mod` v2 == 0

mkExprs :: [Value] -> [(Expr, Value)]
mkExprs [x] = [(Num x, x)]
mkExprs xs = [ev | (ys,zs) <- unmerges xs
                  ,ev1 <- mkExprs ys
                  ,ev2 <- mkExprs zs
                  ,ev  <- combine ev1 ev2]

unmerges :: [a] -> [([a],[a])]
unmerges [x,y] = [([x],[y])]
unmerges (x:xs) = [([x],xs), (xs,[x])] 
                  ++ map (applyFst (x:)) (unmerges xs)
                  ++ map (applySnd (x:)) (unmerges xs)

combine :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
combine (e1,v1) (e2,v2) = [(App op e1 e2, apply op v1 v2) | op <- [Add,Sub,Mul,Div], legal op v1 v2]

nearest :: Int -> [(Expr, Value)] -> (Expr, Value)
-- nearest n ts = minimumBy (compare `on` snd) [(e, abs $ v - n) | (e,v) <- ts]
nearest n ((e,v):evs) = if d == 0 then (e,v)
                                  else search n d (e,v) evs
                          where d = abs (n - v)

search n d ev [] = ev
search n d ev ((e,v):evs) 
  | d' == 0 = (e,v)
  | d' < d  = search n d' (e,v) evs
  | d' >= d = search n d ev evs
  where d' = abs (n - v)



--
applyFst :: (a -> b) -> (a, c) -> (b, c)
applyFst f (x,y) = (f x, y)

applySnd :: (b -> c) -> (a, b) -> (a, c)
applySnd f (x,y) = (x, f y)

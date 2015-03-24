module Countdown4 where

import Data.List
import Data.Function

import Types

countdown4 :: Int -> [Int] -> (Expr, Value)
countdown4 n = nearest n . extract memoise. subseqs

mkExprs :: Memo -> [Value] -> [(Expr, Value)]
mkExprs memo [x] = [(Num x, x)]
mkExprs memo xs = [ev | (ys,zs) <- unmerges xs
                  ,ev1 <- fetch memo ys
                  ,ev2 <- fetch memo zs
                  ,ev  <- combine ev1 ev2]

memoise :: [[Int]] -> Memo
memoise = foldl insert empty

insert memo xs = store xs (mkExprs memo xs) memo

-- same as countdown3
combine :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
combine (e1,v1) (e2,v2)
  | v1 < v2  = comb1 (e1,v1) (e2,v2)
  | v1 == v2 = comb2 (e1,v1) (e2,v2)
  | v1 > v2  = comb1 (e2,v2) (e1,v1)

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

unmerges :: [a] -> [([a],[a])]
unmerges [x,y] = [([x],[y])]
unmerges (x:xs) = [([x],xs), (xs,[x])] 
                  ++ map (applyFst (x:)) (unmerges xs)
                  ++ map (applySnd (x:)) (unmerges xs)

comb1 :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
comb1 (e1,v1) (e2,v2) = (if non Sub e1 && non Sub e2
                          then [(App Add e1 e2, v1+v2) | non Add e2] ++ [(App Sub e2 e1, v2-v1)]
                          else []
                        ) ++
                        (if 1 < v1 && non Div e1 && non Div e2
                          then [(App Mul e1 e2, v1*v2) | non Mul e2] ++ [(App Div e2 e1, q) | r == 0]
                          else []
                        )
                        where (q,r) = divMod v2 v1

comb2 :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
comb2 (e1,v1) (e2,v2) = [(App Add e1 e2, v1+v2) | non Sub e1, non Add e2, non Sub e2] ++
                        (if 1 < v1 && non Div e1 && non Div e2
                          then [(App Mul e1 e2, v1*v2) | non Mul e2] ++ [(App Div e1 e2, 1)]
                          else [])

non :: Op -> Expr -> Bool
non op (Num x) = True
non op1 (App op2 e1 e2) = op1 /= op2



--
applyFst :: (a -> b) -> (a, c) -> (b, c)
applyFst f (x,y) = (f x, y)

applySnd :: (b -> c) -> (a, b) -> (a, c)
applySnd f (x,y) = (x, f y)



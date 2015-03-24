module Countdown2 where

import Data.List
import Data.Function

import Types

legal :: Op -> Value -> Value -> Bool
legal Add v1 v2 = v1 <= v2
legal Sub v1 v2 = v2 < v1
legal Mul v1 v2 = 1 < v1 && v1 <= v2
legal Div v1 v2 = 1 < v2 && v1 `mod` v2 == 0

countdown2 :: Int -> [Int] -> (Expr, Value)
countdown2 n = nearest n . concatMap mkExprs . subseqs

unmerges :: [a] -> [([a],[a])]
unmerges [x,y] = [([x],[y])]
unmerges (x:xs) = [([x],xs)] 
                  ++ map (applyFst (x:)) (unmerges xs)
                  ++ map (applySnd (x:)) (unmerges xs)

-- combine :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
-- combine (e1,v1) (e2,v2) = [(App op e1 e2, apply op v1 v2) | op <- ops, legal op v1 v2]
--                        ++ [(App op e2 e1, apply op v2 v1) | op <- ops, legal op v2 v1]
--                        where ops = [Add,Sub,Mul,Div]

combine :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
combine (e1,v1) (e2,v2)
  | v1 < v2  = comb1 (e1,v1) (e2,v2)
  | v1 == v2 = comb2 (e1,v1) (e2,v2)
  | v1 > v2  = comb1 (e2,v2) (e1,v1)

comb1 :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
comb1 (e1,v1) (e2,v2) = [(App Add e1 e2, v1+v2), (App Sub e1 e2, v2-v1)] ++
                        if 1 < v1
                          then [(App Mul e1 e2, v1*v2)] ++ [(App Div e1 e2, q) | r == 0]
                          else []
                        where (q,r) = divMod v2 v1

comb2 :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
comb2 (e1,v1) (e2,v2) = [(App Add e1 e2, v1+v2)] ++
                        if 1 < v1
                          then [(App Mul e1 e2, v1*v2)] ++ [(App Div e1 e2, 1)]
                          else []

-- same as countdown1
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

mkExprs :: [Value] -> [(Expr, Value)]
mkExprs [x] = [(Num x, x)]
mkExprs xs = [ev | (ys,zs) <- unmerges xs
                  ,ev1 <- mkExprs ys
                  ,ev2 <- mkExprs zs
                  ,ev  <- combine ev1 ev2]

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


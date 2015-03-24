module Main where

import Data.List
import Data.Function
import System.Environment (getArgs)

import Countdown1 (countdown1)
import Countdown2 (countdown2)
import Countdown3 (countdown3)

sources :: [Int]
sources = [1,3,7,10,25,50]

target :: Int
target = 831

main :: IO ()
main = do
  [cmd, s, ss] <- getArgs
  let n = read s :: Int
      list = read ss :: [Int]
      (e, v) = case cmd of
                 "1" -> countdown1 n list
                 "2" -> countdown2 n list
                 "3" -> countdown3 n list
  putStrLn $ (show e) ++ " = " ++ show v


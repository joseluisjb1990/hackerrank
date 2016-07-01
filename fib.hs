module Main where

fib n =
    if n == 1 then
      0
    else if n == 0 then
      0
    else if n == 0 then
      1
    else
      fib(n-1) + fib(n-2)


-- This part is related to the Input/Output and can be used as it is
-- Do not modify it
main = do
    input <- getLine
    print . fib . (read :: String -> Int) $ input

import Data.List
import Control.Monad

getPair :: [String] -> (Int, Int)
getPair (x:(y:_)) = (read x, read y)

readPair :: IO((Int, Int))
readPair = do
  pair <- getLine
  return $ getPair $ words pair

getIntArray :: String -> [Int]
getIntArray = (map read) . words

prettyPrinting :: [Int] -> String
prettyPrinting xs =
  let res = unwords $ map show $ xs in
    if null res then
      "-1"
    else
      res

filterR' :: [Int] -> Int -> (Int, [Int]) -> (Int, [Int])
filterR' [] _        (n, ys)     = (n, reverse ys)
filterR' (x': xs) x  (n, ys) = if x' == x then filterR' xs x (n+1, ys) else filterR' xs x (n, x':ys)

filterR :: [Int] -> Int -> [Int] -> [Int]
filterR [] _ res = res
filterR (x:xs) k res =
  let (n, ys) = filterR' xs x (1, [])
  in
    if n >= k then
      filterR ys k (x:res)
    else
      filterR ys k res

processLine = do
  (_, n) <- readPair
  xs <- getLine
  let xs' = getIntArray xs in
    putStrLn $ prettyPrinting $ reverse $ filterR xs' n []


main = do
  n <- getLine
  replicateM (read n) processLine

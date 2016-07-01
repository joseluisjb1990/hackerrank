import Control.Monad

getPair :: [String] -> (Int, Int)
getPair (x:(y:_)) = (read x, read y)

readPair :: IO((Int, Int))
readPair = do
  pair <- getLine
  return $ getPair $ words pair


readFunction :: IO([(Int, Int)])
readFunction = do
  n <- getLine
  replicateM (read n) readPair

readFunctions :: IO([[(Int, Int)]])
readFunctions = do
  n <- getLine
  replicateM (read n) readFunction

unique :: [Int] -> Bool
unique []     = True
unique (x:xs) = (all (/=x) xs) && unique xs

checkFunction :: [(Int, Int)] -> Bool
checkFunction xs = unique $ map fst xs

main = do
  xs <- readFunctions
  mapM_ putStrLn $ map (\x -> if x then "YES" else "NO") $ map checkFunction xs

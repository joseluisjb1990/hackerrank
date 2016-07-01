fact :: Int -> Int
fact 0 = 0
fact n = product [1..n]

getElem :: Int -> Int -> Int
getElem n r =
  let divi = (fact r) * (fact (n-r)) in
    if divi == 0 then
      1
    else
      div (fact n) divi

getRow :: Int -> [Int]
getRow n = 1 : map (getElem n) [1..n]

getPascal :: Int -> [[Int]]
getPascal 0 = [[1]]
getPascal n = [1] : map getRow [1..n-1]

main = do
  n <- getLine
  mapM_ (\l -> putStrLn $ unwords $ map show l) $ getPascal (read n)

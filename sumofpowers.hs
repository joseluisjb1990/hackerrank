checkSum' :: Int -> Int -> Int -> Int -> Int
checkSum' acc r n i =
  let n' = acc + i^r in
    if n' == n then 1
    else
      if n' < n then
        sum $ map (checkSum' n' r n) $ takeWhile (\x -> floor((fromIntegral x)^r) <= n) [i+1..]
      else
        0

checkSum :: Int -> Int -> Int
checkSum n r =
  sum $ map (checkSum' 0 r n) $ takeWhile (\x -> floor((fromIntegral x)^r) <= n) [1..]

main = do
  n <- getLine
  r <- getLine
  putStrLn $ show $ checkSum (read n) (read r)

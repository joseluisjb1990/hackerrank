import Data.List

getPrefix :: String -> String -> [String]
getPrefix xs ys = let n = length $ takeWhile (\(x, y) -> x == y) $ zip xs ys in
  [take n xs, drop n xs, drop n ys]

formatString :: String -> String
formatString xs = (show $ length xs) ++ " " ++ xs

main = do
  xs <- getLine
  ys <- getLine

  mapM putStrLn $ map formatString $ getPrefix xs ys

main = do
  xs <- getLine
  ys <- getLine
  print $ reverse $ foldl (\acc (x, y) -> y:x:acc) [] $ zip xs ys

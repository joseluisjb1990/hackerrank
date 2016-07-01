import Control.Monad

getPair :: [String] -> (Int, Int)
getPair (x:(y:_)) = (read x, read y)

readPair :: IO((Int, Int))
readPair = do
  pair <- getLine
  return $ getPair $ words pair

readPolygon :: IO([(Int, Int)])
readPolygon = do
  n <- getLine
  replicateM (read n) readPair

distance :: (Int, Int) -> (Int, Int) -> Double
distance (x1, y1) (x2, y2) = sqrt ((fromIntegral x2 - fromIntegral x1)^2 + (fromIntegral y2 - fromIntegral y1)^2)

calcArea :: (Int, Int) -> [(Int, Int)] -> Double
calcArea _ []       = 0.0
calcArea z [x]      = distance x z
calcArea z (x:y:xs) = (distance x y) + (calcArea z (y:xs))

main = do
  xs <- readPolygon
  putStrLn $ show $ calcArea (head xs) xs

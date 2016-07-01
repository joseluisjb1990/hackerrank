import Control.Monad
import Text.Printf

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
distance (x1, y1) (x2, y2) =  (fromIntegral x1 * fromIntegral y2) - (fromIntegral y1 * fromIntegral x2)

calcP :: (Int, Int) -> [(Int, Int)] -> Double
calcP _ []       = 0.0
calcP z [x]      = distance x z
calcP z (x:y:xs) = (distance x y) + (calcP z (y:xs))

main = do
  xs <- readPolygon
  printf "%.1f\n" $ abs $ (calcP (head xs) xs) / 2.0

import Control.Monad

permuteString :: String -> String
permuteString [] = []
permuteString (x:y:xs) = y:x:(permuteString xs)

main = do
  n <- getLine
  replicateM (read n) ((liftM permuteString getLine)  >>= putStrLn)

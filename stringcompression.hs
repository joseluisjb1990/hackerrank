import Control.Monad
import Data.List

transformString :: String -> String
transformString xs = if (length xs) == 1 then xs else (head xs) : (show (length xs))

compressString :: String -> String
compressString xs = concatMap transformString $ group xs

main = do
  (liftM compressString getLine)  >>= putStrLn

import Control.Monad.Error
import Data.List

type Height = Int
type Width = Int

data Picture = Picture {
  height :: Height,
  width :: Width,
  pixels :: [[Char]]
} deriving (Show)

pixel :: Char -> Picture
pixel c = Picture 1 1 [[c]]

above :: Picture -> Picture -> Picture
above p0 p1 =
  let wp0 = width p0
      wp1 = width p1
  in
    if wp0 /= wp1 then
      error "can’t ’above’ different widths"
    else
      Picture ((height p0) + (height p1)) wp0 $ foldr (:) (pixels p1) (pixels p0)

beside :: Picture -> Picture -> Picture
beside p0 p1 =
  let hp0 = height p0
      hp1 = height p1
  in
    if hp0 /= hp1 then
      error "can’t ’beside’ different heights"
    else
      Picture hp0 ((width p0) + (width p1)) $ map (\(l0, l1) -> foldr (:) l1 l0) $ zip (pixels p0) (pixels p1)

toString :: Picture -> String
toString = concat . (intersperse "\n") . pixels

stack :: [Picture] -> Picture
stack = foldl1 above

spread :: [Picture] -> Picture
spread = foldl1 beside

row :: String -> Picture
row = spread . (map pixel)

blank :: (Height,Width) -> Picture
blank = uncurry blank'
  where
    blank' h w= stack $ take h $ repeat $ spread $ ((flip take) . repeat) (pixel ' ') w

main = do
  putStrLn $ toString $ spread [(pixel 'x'), (pixel 'b'), (pixel 'a')]
  putStrLn $ toString $ row "qweudfsdhbfklsdhhsf"
  putStrLn $ show $ blank (5, 3)

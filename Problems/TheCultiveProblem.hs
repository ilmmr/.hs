{- ---------------------------------------- -}
module Mining ( tmp ) where
import Data.List
import Data.Tuple

type Matriz a = [[a]]
type Position = (Int, Int)

split :: (a -> b) -> (a -> c) -> a -> (b,c)
split f g x = (f x, g x)

(><) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
f >< g = split (f . fst) (g . snd)

matrix_size :: Matriz Int -> (Int, Int)
matrix_size = split (length . head) length

around :: Position -> (Int, Int) -> [Position]
around (x,y) (n, m) = filter ( f (n, m)) . concat $ [
  return (x-1,y),
  return (x+1,y),
  return (x,y+1),
  return (x,y-1)] where f (n, m) = (\p -> fst p >= 0 && snd p >= 0 && fst p < n && snd p < m)

position_calculation :: Matriz Int -> Position -> Int
position_calculation m = uncurry (*) . split (index m) ((.) ((.) (sum) . map . index $ m) . flip around . matrix_size $ m)
{-
  position_calculation \m pos = (index m pos) * (sum . map (index m) $ (around pos (matrix_size m)))
  position_calculation \m pos = (*) (index m pos) (sum . map (index m) $ around pos (matrix_size m))
  position_calculation \m pos = (*) (index m pos) (sum . map (index m) . flip around (matrix_size m) $ pos)
  position_calculation \m pos =  uncurry (*) $ split (index m pos) (sum . map (index m) . flip around (matrix_size m)) $ pos
  position_calculation \m     = uncurry (*) . split (index m) (sum . map (index m) . flip around (matrix_size m))
  position_calculation \m     = uncurry (*) . split (index m) ((.) (sum . map (index m)) . flip around . matrix_size $ m)
  position_calculation \m     = uncurry (*) . split (index m) ((.) ((.) (sum) (map (index m))) . flip around . matrix_size $ m)
  position_calculation \m     = uncurry (*) . split (index m) ((.) ((.) (sum) . map . index $ m) . flip around . matrix_size $ m)
-}

index :: Matriz Int -> Position -> Int
index = (uncurry (!!) .) . flip split fst . flip (.) snd . (!!)
{-
  index \m pos = (!!) ((!!) m (fst pos)) (snd pos)
  index \m pos = uncurry (!!) $ split ((!!) m . fst) (snd) $ pos
  index \m     = uncurry (!!) . split ((!!) m . fst) (snd)
  index \m     = uncurry (!!) . flip split snd (flip (.) fst ((!!) m))
  index \m     = (uncurry (!!) .) (flip split snd (flip (.) fst ((!!) m)))
  index \m     = (uncurry (!!) .) . flip split snd $ flip (.) fst ((!!) m)
  index \m     = (uncurry (!!) .) . flip split snd $ flip (.) fst . (!!) $ m
  index \m     = (uncurry (!!) .) . flip split snd . flip (.) fst . (!!) $ m
  index        = (uncurry (!!) .) . flip split snd . flip (.) fst . (!!)
-}

calculate :: Matriz Int -> Matriz Int
calculate m = [[(position_calculation m (x, y)) | x <- [0..rows-1]] | y <- [0..cols-1]]
  where
    rows = snd . matrix_size $ m
    cols = fst . matrix_size $ m

build :: Matriz Char -> Matriz Int
build = map . map $ convert 
    where   convert '#' = 0
            convert '.' = 1

{- Input: Matriz [['#', '.'] ...], where '#' means occupied and '.' means free. -}
-- main :: Matriz Char -> ([Position], Int) 
-- main = uncurry (><) (id, length) . f . calculate . build where f n = (,) n n
{- 
    main \m = (><) id length (f (calculate (build m))) where f n = (,) n n
    main \m = (><) id length (f (calculate (build m))) where f n = (,) n n
    main \m = (><) id length $ f $ calculate . build $ m where f n = (,) n n
    main    = (><) id length . f . calculate . build where f n = (,) n n 
-}

tmp :: Matriz Char -> Matriz Int
tmp = calculate . build

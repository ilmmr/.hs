{- ---------------------------------------- -}
module Mining ( main, teste ) where
import Data.List
import Data.Tuple

type Matriz a = [[a]]
type Par a    = (a,a)

split :: (a -> b) -> (a -> c) -> a -> (b,c)
split f g x = (f x, g x)

(><) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
f >< g = split (f . fst) (g . snd)

calculate :: Matriz Int -> [Par Int]
calculate m = undef 

build :: Matriz Char -> Matriz Int
build = map . map convert 
    where   convert '#' = -1
            convert '.' = 0

{- Input: Matriz [['#', '.'] ...], where '#' means occupied and '.' means free. -}
main :: Matriz Char -> ([Par Int], Int) 
main = uncurry (><) (id, length) . f . calculate . build where f n = (,) n n
{- 
    main \m = (><) id length (f (calculate (build m))) where f n = (,) n n
    main \m = (><) id length (f (calculate (build m))) where f n = (,) n n
    main \m = (><) id length $ f $ calculate . build $ m where f n = (,) n n
    main    = (><) id length . f . calculate . build where f n = (,) n n 
-}

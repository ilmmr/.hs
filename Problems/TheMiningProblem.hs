{- ---------- The Mining Problem ---------- -}
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

create :: Int -> Matriz Int
create = (uncurry $ flip replicate . (flip replicate 0)) . f where f n = (,) n n

around :: Par Int -> Int -> [Par Int]
around (x,y) n = filter ( f n ) . concat $ [
  return (x-1,y+1),
  return (x+1,y+1),
  return (x-1,y-1),
  return (x+1,y-1),
  return (x-1,y),
  return (x+1,y),
  return (x,y+1),
  return (x,y-1)] where f n = (\p -> p >= (0,0) && fst p < n && snd p < n)

update :: [Par Int] -> Matriz Int -> Matriz Int
update [] matrix   = matrix
update ((x,y):rr) matrix = update rr (r1 (x,y) matrix) where
              r1 (x,y) (l:r) = if y > 0 then l : r1 (x,y-1) r else ((r2 x l):r) where
                r2 x (h:t) = if x > 0 then h : r2 (x-1) t else (if h /= -1 then (:) ((+1) h) t else (:) h t)

increment :: Par Int -> Matriz Int -> Matriz Int
increment = flip (.) f . uncurry $ flip (.) length . (update .) . around where f n = (,) n n

build :: [Par Int] -> Matriz Int -> Matriz Int
build [] matrix = matrix
build ((x,y) : remaing) matrix = build remaing ((replace1 (x,y) (increment (x,y) matrix))) where
            replace1 (x,y) (l:r) = if y > 0 then l : replace1 (x,y-1) r else ((replace2 x l):r) where
              replace2 x (h:t) = if x > 0 then h : replace2 (x-1) t else (:) (-1) t

main :: [Int] -> [Int] -> Int -> Matriz Int
main = (flip (.) create .) . (build .) . zip

{- ----- run test in ghci ----- -}
teste = main [1,2] [2,1] 3
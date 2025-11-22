module Problems where
import Control.Monad (replicateM)
import Data.List
import Data.Tuple
import Data.Ord (comparing)
import Data.Char
import System.Random

-- p46 ::
type Prop = Bool
and', or', eq' :: Prop -> Prop -> Prop
not'           :: Prop -> Prop
and' = (&&)
or'  = (||)
eq'  = (==)
not' = not
-- p47 ::
infixl 4 `or'`
infixl 6 `and'`

-- p46 (\a b -> (and' a (or' a b))) => Truth table 
p46 :: (Prop -> Prop -> Prop) -> IO ()
p46 f = putStrLn $ concatMap (++ "\n" ) [show a ++ " " ++ show b ++ " -> " ++ show (f a b) | a <- [True, False], b <- [True, False]]

--- p48 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c) => Truth table
p48 :: Int -> ([Bool] -> Bool) -> IO ()
p48 n f = mapM_ putStrLn [fstr a ++ " -> " ++ show (f a) | a <- choices n] where
                choices = mapM (const [True, False]) . enumFromTo 1
                fstr    = let g = map ((++) (" ") . show) in concat . g

--- p49 (n-bit Gray code) 3 = ["000","001","010","011","100","101","110","111"]
p49 :: Int -> [String]
p49 = mapM (const ['0', '1']) . enumFromTo 1

-- p35 55 = [5,11]; p39 9 = [3,3] -> Only primes
p35 :: Int -> [Int]
p35 1 = []
p35 n =  (head (primes n)) : (p35 (n `div` (head (primes n))))
    where primes n   = filter (\x -> mod x 2 /= 0 && x `elem` (divisors n)) [2..n] 
          divisors n = filter (\x -> mod n x == 0) [2..n]

-- primesR 1 11 -> [2,3,5,7,11]
primesR :: Int -> Int -> [Int]
primesR = (filter (isPrime) .) . enumFromTo
        where isPrime n = (==) [1,n] (filter (\x -> mod n x == 0) [1..n])

-- coprime :: both primes and share only one divisor [1]
coprime :: Int -> Int -> Bool
coprime = flip (.) get . ((==) [1] .) . intersect . get where
            get n = (filter (\x -> mod n x == 0) [1..n])

-- totient :: lenght of the list of coprimes to n, that are leq to n
totient :: Int -> Int
totient n = length . filter (\x -> coprime x n) . enumFromTo 1 . pred $ n

-- goldbach 55 = (55,3) :: Combines every combination of primes between 2 and n in a list, and then checks if the sum of the pair equals to n, returning the head of the list.
goldbach :: Int -> (Int, Int)
goldbach n = head . filter (\(x,y) -> x+y == n) $ combPairs (primesR 2 n) where
            combPairs = (uncurry $ (<*>) . ((,) <$>)) . f where f l = (,) l l

-- goldbachL 9 20 = [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)] goldbach of every even number between n and m
goldbachL :: Int -> Int -> [(Int, Int)]
goldbachL = (.) (map goldbach . filter even) . enumFromTo

-- goldbachL' :: same as goldbachL but now starting in a even number greater than x
goldbachL' :: Int -> Int -> Int -> [(Int, Int)]
goldbachL' n m f = filter (flip (>) f . fst) (goldbachL n m)

-- queens :: The queens can not be on the same line, neither the same column and neither the same diagonal -> solution: consider a list of numbers where each index equals a column and a number equals a line
queens :: Int -> [[Int]]
queens n = g $ filter ((uncurry $ (==) . nub) . f) (replicateM n [1..n])
        where 
              g               = filter (check)
              check []        = True
              check (h:r)     = if (any (\(dist, q) -> abs (h - q) == dist) $ zip [1..] r) then False else check r 
              f n = (,) n n

-- knights quest :: calculate the path starting at (1,1) on how the knight can go to each position of the board
knights :: Int -> [[(Int,Int)]]
knights n = loop (n*n) ([[(1,1)]])
    where loop 1 = map reverse . id
          loop i = loop (i-1) . concatMap nextMoves

          nextMoves already@(x:xs) = [next:already | next <- possible]
              where possible = filter (\x -> on_board x && (x `notElem` already)) $ jumps x

          jumps (x,y)    = [(x+a, y+b) | (a,b) <- [(1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2)]]
          on_board (x,y) = (x >= 1) && (x <= n) && (y >= 1) && (y <= n)

-- Trash (??)
-- only arithmetic (+) and (-) because of assoc
equation l = let l' = reverse l in resolve $ concat (map equation $ (head l, tail l) : [(head l', reverse . tail $ l')])
                where equation (l, r) = map (flip (++) ("=" ++ show l)) (combinations . group . map show $ r)
                      resolve         = id
combine l1 l2 = concat (f <$> l1 <*> l2) where f i j = (concat $ intersperse "+" [i,j]) : [(concat $ intersperse "-" [i,j])]
combinations [y] = y
combinations (x:y:tail) = combinations ((combine x y):tail)

-- fullwords 375 = "three-seven-five"
fullwords :: Int -> String
fullwords = apply . (concat . (map (map digitToInt))) . (map (flip (:) []) . show) where
            apply = concat . intersperse "-" . (map ((!!) f))
            f = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
{-
fw :: Integer -> String
fw n = concat $ intersperse "-" [digits!!digitToInt d | d <- show n]
  where digits = ["zero", "one", "two", "three", "four",
                  "five", "six", "seven", "eight", "nine"]
-}

-- CP funcs
splitv :: (a -> b) -> (a -> c) -> a -> (b,c)
splitv f g x = (f x, g x)

(><) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
f >< g = splitv (f . fst) (g . snd)

-- idt "ola-td" = True; idt "ola-" = False; :: identifier valid function
idt :: String -> Bool
idt = uncurry check . splitv (filter (not . isAlphaNum . snd) . (zip [0..])) (pred . length)
        where check l n = (conseq $ map fst l) && (all (flip inedge n) $ map fst l)
              conseq l  = if l /= [] then not . any ((uncurry (==)) . ((><) succ id)) $ (zip l (tail l)) else True
              inedge x  = not . (||) ((==) 0 x) . ((==) x)
{-
    import Text.RegexPR
    import Data.Maybe
    identifier = isJust . matchRegexPR "^[a-zA-Z](-?[a-zA-Z0-9])*$"
-}

-- (flip (.) (enumFromTo 1)) . takerandom 
-- takerandom n r :: take n numbers of a random list of 1 to n
takerandom :: Int -> Int -> IO [Int]
takerandom n r = take n . nub . randomRs (1, r) <$> getStdGen

-- rnd_select s n :: select a random substring (length n) of s
rnd_select :: String -> Int -> IO ()
rnd_select lst n = flip (>>=) putStrLn $ map (lst !!) <$> indices
    where indices = take n . nub . randomRs (0, length lst - 1) <$> getStdGen

-- test
fmap_test :: [Int] -> [Int]
fmap_test = (<$>) (+ 2)

-- sgroup [1,2,3] "asdasda" = ["a","sd","asd"]; first takes 1, then takes 2, then takes 3
sgroup :: [Int] -> [a] -> [[a]]
sgroup [] a    = []
sgroup ist lst = (:) (take (head ist) lst) (sgroup (drop 1 ist) (drop (head ist) lst)) 

-- sort ["abc","de","fgh","de","ijkl","mn","o"] = ["o","de","de","mn","abc","fgh","ijkl"]
sort0 :: [String] -> [String]
sort0 =  map fst . sortBy cmp . (map f) where
         cmp = flip (.) snd . compare . snd
         f   = (\x -> (x, length x))
sort1 = sortBy (comparing length)
-- or sort1 = sortOn length

-- sort2 :: same as sort0 but by frequency; sort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"] = ["ijkl","o","abc","fgh","de","de","mn"]
sort2 :: [String] -> [String]
sort2 = concat . (map sort) . sort1 . groupBy f . reverse . sort1 where
        f = (flip (.) length . (==) . length)

-- insertAt 'X' "abcd" 2 = "aXbcd"
insertAt :: a -> [a] -> Int -> [a]
insertAt c str i = uncurry (++) (ap c (Data.List.splitAt (i-1) str)) where
                   ap = (><) id . (:)

unique :: (Ord a) => [a] -> [a]
unique = map head . group . sort 
{- 
    unique \lst = map head $ group $ sort lst
    unique \lst = map head . group $ sort lst
    unique \lst = map head . group . sort $ lst
    unique      = map head . group . sort 
-}

splitstr :: (Eq a, Ord a) => [a] -> [a] -> [[a]]
splitstr str sep = unique . foldr f [[]] $ str
    where f c acc = if (==) sep . take (length sep) . head $ acc 
                    then [c] : ((drop (length sep) (head acc)) : tail acc) 
                    else uncurry (flip (.) tail) . splitv ((:) . ((.) ((:) c) head)) id $ acc
{- 
    f \c acc = [c] : ((drop (length sep) (head acc)) : tail acc)
-}
module Graphs where
import Data.List
import Data.Tuple

{-
    HOW TO REPRESENT A GRAPH?
    
    -> Graph     :: Nodes and Edges
    -> Adjacency :: List of nodes where each node has a list of connected pairs, like a map in python
    -> Friendly  :: [b-c, f-c, g-h, d, f-b, k-f, h-g] where the atoms stand for isolated nodes, the X-Y terms describe edges.
-}

data Graph a = Graph [a] [(a, a)]
               deriving (Show, Eq)
			  
data Adjacency a = Adj [(a, [a])]
		   deriving (Show, Eq)

data Friendly a = Edge [(a, a)]
		  deriving (Show, Eq)

-- graphToAdj Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')] = Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]
graph2adj :: (Eq a) => Graph a -> Adjacency a
graph2adj (Graph l edges) = Adj $ map ((\x -> (x, process x edges))) l
                            where process x edges = (++) (map snd $ filter ((==) x . fst) edges) (map fst $ filter ((==) x . snd) edges)

-- adj2graph (Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]) = Graph "bcdfghk" [('c','b'),('f','b'),('f','c'),('h','g'),('k','f')]
adj2graph :: (Eq a) => Adjacency a -> Graph a
adj2graph (Adj list) = Graph (nub . map fst $ list) (clear . nub . concat . map seperate $ list)
                        where clear           = foldr (\x xs -> if swap x `elem` xs then xs else x : xs) []
                              seperate (x, y) = map ((,) x) y

-- discover a path giving to nodes a b and a graph (a->...->b)
paths :: (Eq a) => a -> a -> [(a,a)] -> [[a]]
paths a b g = if a /= b then if a `elem` (map fst g) then let l = map snd $ filter ((==) a . fst) g in ap a $ concatMap (\x -> paths x b g) l else [] else [[a]]
                where ap = map . (:)

-- discover a cycle in the graph given a node: cycle 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
cycle :: (Eq a) => a -> [(a,a)] -> [[a]]
cycle = (uncurry paths) . f where f n = (,) n n
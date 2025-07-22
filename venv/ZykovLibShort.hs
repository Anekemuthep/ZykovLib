import Data.List
import Text.Printf (printf)
import System.IO
import Data.Char (isSpace)
import Language.Haskell.Interpreter

main :: IO ()
main = do
  -- Define the Haskell code as a string
  let haskellCode = "module MyModule () where\n\nfactorial :: Int -> Int\nfactorial 0 = 1\nfactorial n = n * factorial (n-1)"

  -- Run the Haskell code using the GHC interpreter
  result <- runInterpreter $ do
    setImports ["Prelude"]
    setTopLevelModules ["MyModule"]
    loadModules ["MyModule.hs"]
    setImportsQ [("MyModule", Just "MyModule")]
    interpret "factorial 5" (as :: Int)

  -- Print the result of the evaluation
  case result of
    Left err -> print $ "Error: " ++ show err
    Right x -> print x


-- Main function
main1 :: IO ()
main1 = do
--    putStrLn "Enter an expression:"
--    input <- getLine
  let inputGraph = ([1,2,3,4,5], [[1,2],[1,3],[2,4],[2,5]])
      inputGraph2 = v 1 .* v 2 .+ v 3 .* v 4
      markdownString = toMermaid inputGraph2
  writeMarkdownFile "document2.md" markdownString

graphToMermaid :: ([Int],[[Int]]) -> String -> IO ()
graphToMermaid graphExp title = do
--    putStrLn "Enter an expression:"
--    input <- getLine
  let markdownString = toMermaid graphExp
  writeMarkdownFile (title ++ ".md") markdownString

-- Convert a graph in adjacency list format to a Mermaid .md file
toMermaid :: ([Int], [[Int]]) -> String
toMermaid (vs, es) = printf "```mermaid\ngraph TD\n%s\n%s\n```\n" nodes edges
  where nodes = unlines $ map (\v -> printf "  %d" v) vs
        edges = unlines $ map (\[u, v] -> printf "  %d --- %d" u v) es

 
main2 :: IO ()
main2 = do
  putStrLn "Enter an expression:"
  input <- getLine
  let output = replaceOps input
  putStrLn $ "Modified expression: " ++ output
 

replaceOps :: String -> String
replaceOps [] = []
replaceOps ('+' : ' ' : xs) = ".+ " ++ replaceOps xs
replaceOps ('*' : ' ' : xs) = ".* " ++ replaceOps xs
replaceOps (x : xs) = x : replaceOps xs

deList :: String -> String
deList [] = []
deList (x:xs)
  | x `elem` "[]," = ' ' : x : ' ' : deList xs
  | x == '+' = ' ' : x : ' ' : deList xs
  | x == '*' = ' ' : x : ' ' : deList xs  | isSpace x = deList xs
  | otherwise = x : deList xs
 
-- Write a string to a Markdown file
writeMarkdownFile :: FilePath -> String -> IO ()
writeMarkdownFile path content = withFile path WriteMode $ \h -> hPutStr h content

--------------
-- Definitions
--------------

-- A. Atomic Elements
---------------------

-- Empty graph o
---------------------
o :: ([a1],[a2])
o = ([],[])

-- Vortex v n
---------------------
v :: a1 -> ([a1],[a2])  
v n = ([n],[])

-- Null of a graph g
---------------------
n :: (a1,b) -> (a1,[a2])
n g = (fst g, [])

-- B. Operations
----------------

-- Hierarchy of operations
infixr 5 .+
infixr 6 .*

-- Overlay
----------------
(.+) :: (Eq a) => ([a],[[a]]) -> ([a],[[a]]) -> ([a],[[a]])
a .+ b = (nub (fst a ++ fst b), nub (snd a ++ snd b))

-- Bustamante-Mokhov identity/engine 2011-2014 (201114)
-------------------------------------------------
--bm201114 :: ([a],[[a]]) -> ([a],[[a]]) -> [[a]]
--bm201114 a b = nub (snd a ++ snd b) ++  unique [[u,v] | u <- fst a, v <- fst b]
-- Link
----------------
--(.*) :: (Eq a) => ([a],[[a]]) -> ([a],[[a]]) -> ([a],[[a]])
(.*) :: Ord a => ([a], [[a]]) -> ([a], [[a]]) -> ([a], [[a]])
a .* b =  (nub (fst a ++ fst b), nub (bm201114 a b))
--       | otherwise      = kom a
--       where bm201114 a b = nub (snd a ++ snd b)
--                          ++  unique [[u,v] | u <- fst a, v <- fst b]

--bm201114 :: (Eq a) => ([a],[[a]]) -> ([a],[[a]]) -> [[a]]
bm201114 a b = sort2 (snd a ++ snd b ++ unique [[u,v] | u <- fst a, v <- fst b])

unique :: Eq a => [[a]] -> [[a]]
unique = filter (\[x, y] -> x /= y)

sort2 xs = sort [sort x | x <- xs]


sortG g = (sort (fst g), sort (snd g))

(.==) :: (Ord a1, Ord a2)
    => ([a1], [a2])
    -> ([a1], [a2])
    -> Bool
-- Takes two graphs and compare
-- them after being sorted.
a .== b = sortG a == sortG b

--k :: (Eq a, Num a, Enum a) => a -> ([a],[[a]])
--k n = foldl (.*) o [v i | i <-[1..n]]

--kom g = foldl (.*) o [v i | i <- fst (n g)]

-- B. Operations

-- Prints

-- Print a graph
----------------
--printGraph :: (Show a, Show a1) => ([a],[[a1]]) -> IO ()
printGraph g = do
  putStrLn $ "Vertices: " ++ show (fst g)
  putStrLn $ "Edges: " ++ show (snd g)

-- Print a graph in adjacency list format
-----------------------------------------
--printGraphAdj :: (Show a, Show a1) => ([a],[[a1]]) -> IO ()
printGraphAdj g = do
  putStrLn $ "Adjacency list: " ++ show (snd g)

-- adjacency matrix
--------------------
adjMatrix :: (Eq a, Num a, Enum a) => ([a],[[a]]) -> [[Int]]
adjMatrix g = [[if elem [u,v] (snd g) then 1 else 0 | v <- fst g] | u <- fst g]

-- Print a graph in adjacency matrix format
-------------------------------------------
--printGraphAdjMatrix :: (Show a, Show a1) => ([a],[[a1]]) -> IO ()
printGraphAdjMatrix g = do
  putStrLn $ "Adjacency matrix: " ++ show (adjMatrix g)

-- Print mermaid graph line by line
----------------------
--printMermaid :: (Show a, Show a1) => ([a],[[a1]]) -> IO ()
printMermaid g = do
  putStrLn $ "```mermaid"
  putStrLn $ "graph TD"
  putStrLn $ unlines $ map (\v -> printf "  %d" v) (fst g)
  putStrLn $ unlines $ map (\[u, v] -> printf "  %d --- %d" u v) (snd g)
  putStrLn $ "```"

-- Algebraic data typed print
----------------------

type Graph a = ([a],[[a]])
data Printer = Simple | Adj | Matrix | Mermaid | All deriving (Show, Eq)

-- Print a graph
----------------
--printGraph' :: (Show a, Show a1) => Printer -> ([a],[[a]]) -> IO ()
printGraph' Simple g = printGraph g
printGraph' Adj g = printGraphAdj g
printGraph' Matrix g = printGraphAdjMatrix g
printGraph' Mermaid g = printMermaid g
printGraph' All g = do
  printGraph g
  printGraphAdj g
  printGraphAdjMatrix g
  printMermaid g


-- Algebraic data type for graphs
---------------------------------
--data Graph a = Empty | Vortex a | Link a a | Overlay (Graph a) (Graph a) | Connect (Graph a) (Graph a) deriving (Show, Eq)

-- Graphs
----------------

-- Empty graph
----------------
--empty :: Graph a
empty = ([],[])
--empty = Empty

-- Vortex
----------------
--vortex :: a -> Graph a
vortex n = ([n],[])
--vortex n = Vortex n

-- Null of a graph
----------------
--null :: Graph a -> Graph a
null g = (fst g, [])
--null g = Null g

-- Overlay
----------------
--overlay :: (Eq a) => Graph a -> Graph a -> Graph a
overlay a b = (nub (fst a ++ fst b), nub (snd a ++ snd b))
--overlay a b = Overlay a b

-- Link
----------------
--link :: (Eq a) => Graph a -> Graph a -> Graph a
--link a b | fst a /= fst b =  (nub (fst a ++ fst b), nub (bm201114 a b))
--         | otherwise      = kom a
--         where bm201114 a b = nub (snd a ++ snd b)
--                            ++  unique [[u,v] | u <- fst a, v <- fst b]

--link a b = Link a b

-- Complement
----------------
--complement :: (Eq a, Num a, Enum a) => Graph a -> Graph a
--complement g = (fst g, [[u,v] | u <- fst g, v <- fst g, u /= v, not (elem [u,v] (snd g))])
--complement g = Complement g

-- Clique
----------------
--clique :: (Eq a, Num a, Enum a) => a -> Graph a
--clique n = foldl link o [v i | i <-[1..n]]
--clique n = Clique n

-- Path
----------------
--path :: (Eq a, Num a, Enum a) => a -> Graph a
--path n = foldl link o [v i | i <-[1..n-1]]
--path n = Path n

-- Cycle
----------------
--cycle :: (Eq a, Num a, Enum a) => a -> Graph a
--cycle n = foldl link o [v i | i <-[1..n-1]] .* link (v 1) (v n)
--cycle n = Cycle n

-- Wheel
----------------
--wheel :: (Eq a, Num a, Enum a) => a -> Graph a
--wheel n = foldl link o [v i | i <-[1..n-1]] .* link (v 1) (v n) .* foldl link o [v i | i <-[2..n]]
--wheel n = Wheel n

-- Star
----------------
--star :: (Eq a, Num a, Enum a) => a -> Graph a
--star n = foldl link o [v i | i <-[2..n]]
--star n = Star n

-- Bipartite
----------------
--bipartite :: (Eq a, Num a, Enum a) => a -> a -> Graph a
--bipartite n m = foldl link o [v i | i <-[1..n]] .* foldl link o [v i | i <-[n+1..n+m]]
--bipartite n m = Bipartite n m

-- Hypercube
----------------
--hypercube :: (Eq a, Num a, Enum a) => a -> Graph a
--hypercube n = foldl link o [v i | i <-[1..2^n]]
--hypercube n = Hypercube n

-- C. Properties
----------------

-- Is the graph empty?
----------------------
--isEmpty :: ([a],[[a]]) -> Bool
isEmpty g = fst g == []

-- Is the graph a vortex?
------------------------
--isVortex :: ([a],[[a]]) -> Bool
isVortex g = fst g /= [] && snd g == []

-- Is the graph a null?
----------------------
--isNull :: ([a],[[a]]) -> Bool
isNull g = snd g == []

-- Is the graph a link?
----------------------
--isLink :: ([a],[[a]]) -> Bool
isLink g = fst g /= [] && snd g /= []

-- Is the graph a complete graph?
--------------------------------
--isComplete :: (Eq a, Num a, Enum a) => ([a],[[a]]) -> Bool
isComplete g = fst g == [1..n] && snd g == [[u,v] | u <- [1..n], v <- [u..n], u /= v]
  where n = fromIntegral (length (fst g))

-- Is the graph a complete bipartite graph?
-------------------------------------------
--isCompleteBipartite :: (Eq a, Num a, Enum a) => ([a],[[a]]) -> Bool
isCompleteBipartite g = fst g == [1..n] && snd g == [[u,v] | u <- [1..n], v <- [n+1..2*n]]
  where n = fromIntegral (length (fst g) `div` 2)

-- Is the graph a complete multipartite graph?
----------------------------------------------
--isCompleteMultipartite :: (Eq a, Num a, Enum a) => ([a],[[a]]) -> Bool 
isCompleteMultipartite g = fst g == [1..n] && snd g == [[u,v] | u <- [1..n], v <- [n+1..2*n]]
  where n = fromIntegral (length (fst g) `div` 2)


-- function that takes the string (v 1 .+ v 2) .+ v 3 and returns the graph
-- ([1,2,3],[[1,2],[1,3],[2,3]])

-- function that takes the string (v 1 .+ v 2) .+ v 3 and returns the graph
-- ([1,2,3],[[1,2],[1,3],[2,3]])

-- function toGraph that takes the string (v 1 .+ v 2) .+ v 3 and returns the graph
-- ([1,2,3],[[1,2],[1,3],[2,3]])

-- function toGraph that takes the string (v 1 .+ v 2) .+ v 3 and returns the graph
-- ([1,2,3],[[1,2],[1,3],[2,3]])

-- function toGraph that takes the string "(v 1 .+ v 2) .* v 3" and returns the graph
-- ([1,2,3],[[1,2],[1,3],[2,3]])

-- function toGraph that takes the string "(v 1 .+ v 2) .* v 3" and returns the graph

toGraph :: String -> ([Int],[[Int]])
toGraph s = read s :: ([Int],[[Int]])

-- a function toVortex that takes the string "v 1" and returns the graph ([1],[])

toVortex :: String -> ([Int],[[Int]])
toVortex s = read s :: ([Int],[[Int]])

factorial :: Int -> Int
factorial n = product [1..n]

-- function toGraph that takes the string "k 3" and returns the graph ([1,2,3],[[1,2],[1,3],[2,3]])

-- function toGraph that takes the string "k 3" and returns the graph ([1,2,3],[[1,2],[1,3],[2,3]])
-- function toGraphK that takes the string "k 3" and returns the graph ([1,2,3],[[1,2],[1,3],[2,3]])

toGraphK :: String -> ([Int],[[Int]])
toGraphK s = read s :: ([Int],[[Int]])

-- function toGraphKom that takes the string "kom (k 3)" and returns the graph ([1,2,3],[[1,2],[1,3],[2,3]])

toGraphKom :: String -> ([Int],[[Int]])
toGraphKom s = read s :: ([Int],[[Int]])






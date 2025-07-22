import Text.Read (readMaybe)
import Data.List
import Text.Printf (printf)
import System.IO
import Data.Char (isSpace)

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


factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

evalFunc :: String -> Maybe Integer
evalFunc str = do
    n <- readMaybe str
    return $ factorial n


main :: IO ()
main = do
    let func = "5"
    case evalFunc func of
        Nothing -> putStrLn "Invalid input"
        Just val -> print val  -- Outputs: 120

printMermaid g = do
  putStrLn $ "```mermaid"
  putStrLn $ "graph TD"
  putStrLn $ unlines $ map (\v -> printf "  %d" v) (fst g)
  putStrLn $ unlines $ map (\[u, v] -> printf "  %d --- %d" u v) (snd g)
  putStrLn $ "```"

evalFunc2 :: String -> Maybe ([Int], [[Int]])
evalFunc2 grf = do
    g <- readMaybe grf
    return $ printMermaid g


main2 :: IO ()
main2 = do
    let func = "v 1 .* v 2"
    case evalFunc2 func of
        Nothing -> putStrLn "Invalid input"
        Just val -> print val  -- Outputs: 120
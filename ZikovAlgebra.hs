import Data.List
import Text.Printf (printf)
import System.IO
import Data.Char (isSpace)

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
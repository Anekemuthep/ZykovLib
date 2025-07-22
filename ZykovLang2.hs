-- ZykovLang: Non-Mokhov Edition
-- Algebraic Graph Composer with Mermaid + PNG Export + MacGyver-Optimized Node Launch 😄
-- Version: v0.7 "Non-Mokhov Edition"

import Data.List
import Text.Printf (printf)
import System.IO
import Data.Char --(isSpace)
import System.Process (callCommand)
import System.Directory (doesFileExist)
import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)

-- Graph type definition
type Graph a = ([a], [[a]])

-- Entry point
main :: IO ()
main = do
  putStrLn "Welcome to ZykovLang — Algebraic Graph Composer"
  putStrLn "Enter a graph expression (e.g. 1*(2+3)+3*4):"
  exprStr <- getLine
  putStrLn "Enter a name for the output file (without extension):"
  filename <- getLine
  
  let graph = eval exprStr
      markdown = toMermaid graph
      mdFile = filename ++ ".md"
      pngFile = filename ++ ".png"
      pngFileM1 = filename ++ "-1.png"  -- Mermaid CLI appends -1.png
  
  writeMarkdownFile mdFile markdown
  putStrLn $ "✅ Mermaid markdown saved to " ++ mdFile

  -- Convert to PNG
  putStrLn "Generating PNG..."
  callCommand $ "mmdc -i " ++ mdFile ++ " -o " ++ pngFile
  putStrLn $ "✅ PNG generated: " ++ pngFile

  -- Attempt to preview
  exists <- doesFileExist pngFileM1
  if exists
    then do
      putStrLn "Opening PNG preview..."
      callCommand $ "open " ++ pngFileM1
    else
      putStrLn "⚠ PNG not found."

--------------------------------------------------
-- Algebra Core
--------------------------------------------------

infixr 5 .+
infixr 6 .*

-- Atomic
v :: a -> Graph a
v n = ([n], [])

o :: Graph a
o = ([], [])

-- Operations
(.+) :: (Eq a) => Graph a -> Graph a -> Graph a
(a1, a2) .+ (b1, b2) = (nub (a1 ++ b1), nub (a2 ++ b2))

(.*) :: (Ord a) => Graph a -> Graph a -> Graph a
(a1, a2) .* (b1, b2)
  | a1 /= b1 = (nub (a1 ++ b1), sort2 (a2 ++ b2 ++ unique [[u, v] | u <- a1, v <- b1]))
  | otherwise = kom (a1, a2)

kom :: (Ord a) => Graph a -> Graph a
kom (a1, _) = foldl (.*) o [v i | i <- a1]

-- Helpers
unique :: Eq a => [[a]] -> [[a]]
unique = filter (\[x, y] -> x /= y)

sort2 :: (Ord a) => [[a]] -> [[a]]
sort2 xs = sort [sort x | x <- xs]

sortG g = (sort (fst g), sort (snd g))

(.==) :: (Ord a1, Ord a2) => ([a1], [a2]) -> ([a1], [a2]) -> Bool
a .== b = sortG a == sortG b

--------------------------------------------------
-- Parser
--------------------------------------------------

digit :: ReadP Int
digit = read <$> munch1 isDigit

charP :: Char -> ReadP Char
charP = char

parens :: ReadP a -> ReadP a
parens p = between (charP '(') (charP ')') p

expr :: ReadP (Graph Int)
expr = chainl1 term addP

term :: ReadP (Graph Int)
term = chainl1 factor mulP

factor :: ReadP (Graph Int)
factor = parens expr +++ vertex

addP :: ReadP (Graph Int -> Graph Int -> Graph Int)
addP = do _ <- charP '+'; return (.+)

mulP :: ReadP (Graph Int -> Graph Int -> Graph Int)
mulP = do _ <- charP '*'; return (.*)

vertex :: ReadP (Graph Int)
vertex = do n <- digit; return (v n)

eval :: String -> Graph Int
eval s = case readP_to_S (expr <* eof) s of
  [(n, "")] -> n
  _          -> error "Invalid expression"

--------------------------------------------------
-- Mermaid Export
--------------------------------------------------

toMermaid :: ([Int], [[Int]]) -> String
toMermaid (vs, es) = printf "```mermaid\ngraph TD\n%s\n%s\n```\n" nodes edges
  where
    nodes = unlines $ map (\v -> printf "  %d" v) vs
    edges = unlines $ map (\[u, v] -> printf "  %d --- %d" u v) es

writeMarkdownFile :: FilePath -> String -> IO ()
writeMarkdownFile path content = withFile path WriteMode $ \h -> hPutStr h content

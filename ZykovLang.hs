-- ZykovLang: Integrated Graph Expression Evaluator and Mermaid Exporter
-- Author: Alfonso Bustamante Valenzuela (with spirit of Zykov whispering nearby)

import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)
import Data.Char
import Data.List (nub, sort)
import System.IO
import System.Process (callCommand)

type Graph a = ([a], [[a]])

-- ==== Parser ====

digit :: ReadP Int
digit = read <$> many1 (satisfy isDigit)

charP :: Char -> ReadP Char
charP = char

manyP :: ReadP a -> ReadP [a]
manyP = many

many1P :: ReadP a -> ReadP [a]
many1P = many1

expr :: ReadP (Graph Int)
expr = chainl1 term addP

term :: ReadP (Graph Int)
term = chainl1 factor mulP

factor :: ReadP (Graph Int)
factor = parens expr +++ vertex

addP :: ReadP (Graph Int -> Graph Int -> Graph Int)
addP = charP '+' >> return (.+)

mulP :: ReadP (Graph Int -> Graph Int -> Graph Int)
mulP = charP '*' >> return (.*)

parens :: ReadP a -> ReadP a
parens p = between (charP '(') (charP ')') p

vertex :: ReadP (Graph Int)
vertex = do
  n <- digit
  return (v n)

eval :: String -> Graph Int
eval s = case readP_to_S (expr <* eof) s of
    [(n, "")] -> n
    _         -> error "Invalid expression"

-- ==== Algebra ====

o :: Graph a
o = ([], [])

v :: a -> Graph a
v n = ([n], [])

(.+) :: (Eq a) => Graph a -> Graph a -> Graph a
(a1, a2) .+ (b1, b2) = (nub (a1 ++ b1), nub (a2 ++ b2))

(.*) :: (Ord a) => Graph a -> Graph a -> Graph a
(a1, a2) .* (b1, b2) = (nub (a1 ++ b1), sort2 (a2 ++ b2 ++ unique [[u,v] | u <- a1, v <- b1]))

kom :: (Ord a) => Graph a -> Graph a
kom (a1, _) = foldl (.*) o [v i | i <- a1]

unique :: Eq a => [[a]] -> [[a]]
unique = filter (\[x, y] -> x /= y)

sort2 :: Ord a => [[a]] -> [[a]]
sort2 xs = sort [sort x | x <- xs]

-- ==== Mermaid Generator ====

toMermaid :: (Show a) => Graph a -> String
toMermaid (vs, es) = 
  "```mermaid\ngraph TD\n"
  ++ unlines (map (\v -> "  " ++ show v) vs)
  ++ unlines (map (\[u,v] -> "  " ++ show u ++ " --- " ++ show v) es)
  ++ "```\n"

writeMarkdownFile :: FilePath -> String -> IO ()
writeMarkdownFile path content = withFile path WriteMode $ \h -> hPutStr h content

-- ==== Main ====

main :: IO ()
main = do
  putStrLn "Welcome to ZykovLang — Algebraic Graph Composer"
  putStrLn "Enter a graph expression (e.g. 1*(2+3)+3*4):"
  input <- getLine
  putStrLn "Enter a name for the output file (without extension):"
  filename <- getLine
  let graph = eval input
      content = toMermaid graph
  writeMarkdownFile (filename ++ ".md") content
  putStrLn $ "Mermaid graph written to " ++ filename ++ ".md"
  -- Generar PNG usando Mermaid CLI
  let pngFile = filename ++ ".png"
  let mmdcCmd = "mmdc -i " ++ filename ++ ".md -o " ++ pngFile
  putStrLn "🌀 Generating PNG using Mermaid CLI..."
  callCommand mmdcCmd

  putStrLn $ "✅ PNG image generated to you: " ++ pngFile
  -- justo después de generar el PNG
  callCommand $ "open " ++ filename ++ "-1.png"
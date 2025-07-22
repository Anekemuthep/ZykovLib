-- ZykovLang: Non-Mokhov Edition
-- Algebraic Graph Composer with Mermaid + PNG Export + MacGyver-Optimized Node Launch 😄
-- Version: v0.9 "Labelled Edition" (Supports labeled nodes with optional metadata)

import Data.List
import Text.Printf (printf)
import System.IO
import Data.Char (isAlphaNum, isSpace)
import System.Process (callCommand)
import System.Directory (doesFileExist)
import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)

-- Graph type definition with metadata support
type Node = (String, Maybe String) -- (label, optional type or category)
type Edge = [String]
type Graph = ([Node], [Edge])

-- Entry point
main :: IO ()
main = do
  putStrLn "Welcome to ZykovLang — Algebraic Graph Composer with Labels"
  putStrLn "Enter a graph expression (e.g. A:person*(B:idea+C:tool)+C*D):"
  exprStr <- getLine
  putStrLn "Enter a name for the output file (without extension):"
  filename <- getLine

  let graph = eval exprStr
      markdown = toMermaid graph
      mdFile = filename ++ ".md"
      pngFile = filename ++ ".png"
      pngFileM1 = filename ++ "-1.png"

  writeMarkdownFile mdFile markdown
  putStrLn $ "✅ Mermaid markdown saved to " ++ mdFile

  putStrLn "Generating PNG..."
  callCommand $ "mmdc -i " ++ mdFile ++ " -o " ++ pngFile
  putStrLn $ "✅ PNG generated: " ++ pngFileM1

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

v :: String -> Graph
v n = ([parseNode n], [])

parseNode :: String -> Node
parseNode s = case span (/= ':') s of
  (lbl, ':' : meta) -> (lbl, Just meta)
  (lbl, _)          -> (lbl, Nothing)

o :: Graph
o = ([], [])

(.+) :: Graph -> Graph -> Graph
(a1, a2) .+ (b1, b2) = (nub (a1 ++ b1), nub (a2 ++ b2))

(.*) :: Graph -> Graph -> Graph
(a1, a2) .* (b1, b2)
  | map fst a1 /= map fst b1 = (nub (a1 ++ b1), sort2 (a2 ++ b2 ++ unique [[fst u, fst v] | u <- a1, v <- b1]))
  | otherwise = kom (a1, a2)

kom :: ([Node], [Edge]) -> Graph
kom (a1, _) = foldl (.*) o [([n], []) | n <- a1]

unique :: Eq a => [[a]] -> [[a]]
unique = filter (\[x, y] -> x /= y)

sort2 :: (Ord a) => [[a]] -> [[a]]
sort2 xs = sort [sort x | x <- xs]

sortG g = (sort (fst g), sort (snd g))

(.==) :: Graph -> Graph -> Bool
a .== b = sortG a == sortG b

--------------------------------------------------
-- Parser (now supports alphanumeric node labels and optional metadata)
--------------------------------------------------

charP :: Char -> ReadP Char
charP = char

parens :: ReadP a -> ReadP a
parens p = between (charP '(') (charP ')') p

expr :: ReadP Graph
expr = chainl1 term addP

term :: ReadP Graph
term = chainl1 factor mulP

factor :: ReadP Graph
factor = parens expr +++ vertex

addP :: ReadP (Graph -> Graph -> Graph)
addP = do _ <- charP '+'; return (.+)

mulP :: ReadP (Graph -> Graph -> Graph)
mulP = do _ <- charP '*'; return (.*)

vertex :: ReadP Graph
vertex = do lbl <- munch1 (\c -> isAlphaNum c || c == ':' ); return (v lbl)

eval :: String -> Graph
eval s = case readP_to_S (expr <* skipSpaces <* eof) s of
  [(n, "")] -> n
  _          -> error "Invalid expression"

--------------------------------------------------
-- Mermaid Export
--------------------------------------------------

toMermaid :: Graph -> String
toMermaid (vs, es) = printf "```mermaid\ngraph TD\n%s\n%s\n```\n" nodes edges
  where
    nodes = unlines $ map formatNode vs
    edges = unlines $ map formatEdge es

formatNode :: Node -> String
formatNode (lbl, Nothing) = printf "  %s" lbl
formatNode (lbl, Just tag) = printf "  %s[\"%s (%s)\"]" lbl lbl tag

formatEdge :: [String] -> String
formatEdge [u, v] = printf "  %s --- %s" u v
formatEdge _ = ""

writeMarkdownFile :: FilePath -> String -> IO ()
writeMarkdownFile path content = withFile path WriteMode $ \h -> hPutStr h content

-- ZykovLang: Non-Mokhov Edition
-- Algebraic Graph Composer with Mermaid + PNG Export + Directed Edge Rules + Node Colors by Type
-- Version: v1.4 "Semantic Color Edition"

import Data.List
import Text.Printf (printf)
import System.IO
import Data.Char (isAlphaNum, isSpace)
import System.Process (callCommand)
import System.Directory (doesFileExist)
import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)
import Data.Maybe (fromMaybe)

-- Graph type definition with metadata and labeled edges
-- (label, optional type)
type Node = (String, Maybe String)
-- (from, to, optional label, directed?)
type Edge = (String, String, Maybe String, Bool)
type Graph = ([Node], [Edge])

-- Entry point
main :: IO ()
main = do
  putStrLn "Welcome to ZykovLang — Algebraic Graph Composer with Labels and Rules"
  putStrLn "Enter a graph expression (e.g. A:actor*(B:director+C:writer)+B*D):"
  exprStr <- getLine
  putStrLn "Enter label rules (e.g. actor,director,movie | actor*director=actor{worked_with}director):"
  rulesStr <- getLine
  putStrLn "Enter a name for the output file (without extension):"
  filename <- getLine

  let baseGraph = eval exprStr
      typedGraph = applyEdgeRules baseGraph (parseRules rulesStr baseGraph)
      markdown = toMermaid typedGraph
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
  | map fst a1 /= map fst b1 = (nub (a1 ++ b1), sort $ nub $ a2 ++ b2 ++ [ (fst u, fst v, Nothing, False) | u <- a1, v <- b1, fst u /= fst v])
  | otherwise = kom (a1, a2)

kom :: ([Node], [Edge]) -> Graph
kom (a1, _) = foldl (.*) o [([n], []) | n <- a1]

--------------------------------------------------
-- Rule Engine (type-based)
--------------------------------------------------

parseRules :: String -> Graph -> [((String, String), (String, Bool))]
parseRules input (nodes, _) =
  let (typesSection, rulesSection) = break (== '|') input
      types = map (filter (not . isSpace)) $ splitOn ',' $ takeWhile (/= '|') input
      rules = splitOn ',' $ drop 1 rulesSection
  in map (parseRuleFromTypes types nodes) rules

parseRuleFromTypes :: [String] -> [Node] -> String -> ((String, String), (String, Bool))
parseRuleFromTypes types nodes s =
  let cleaned = filter (not . isSpace) s
      (lhs, rhsFull) = break (== '=') cleaned
      rhs = drop 1 rhsFull
      (leftType, rightType) = case splitOn '*' lhs of
        [a, b] -> (a, b)
        _ -> ("", "")
      (origin, labelRest) = break (== '{') rhs
      label = takeWhile (/= '}') $ drop 1 labelRest
      (dirFromType, dirToType) = case break (== '{') lhs of
        (a, '*':b) -> (a, b)
        _ -> (leftType, rightType)
      findLabelByType t = [ lbl | (lbl, Just typ) <- nodes, typ == t ]
      leftMatches = findLabelByType leftType
      rightMatches = findLabelByType rightType
  in head [ ((a, b), (label, True)) | a <- leftMatches, b <- rightMatches, typOf a == dirFromType, typOf b == dirToType ]
  where typOf name = fromMaybe "" (lookup name [(lbl, typ) | (lbl, Just typ) <- nodes])

applyEdgeRules :: Graph -> [((String, String), (String, Bool))] -> Graph
applyEdgeRules (vs, es) rules = (vs, map labelEdge es)
  where
    labelEdge (u, v, _, _) = case lookup (u, v) rules of
      Just (lbl, dir) -> (u, v, Just lbl, dir)
      Nothing -> case lookup (v, u) rules of
        Just (lbl, dir) -> (v, u, Just lbl, dir)
        Nothing -> (u, v, Nothing, False)

--------------------------------------------------
-- Parser
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
vertex = do lbl <- munch1 (\c -> isAlphaNum c || c == ':'); return (v lbl)

eval :: String -> Graph
eval s = case readP_to_S (expr <* skipSpaces <* eof) s of
  [(n, "")] -> n
  _          -> error "Invalid expression"

--------------------------------------------------
-- Mermaid Export
--------------------------------------------------

formatNode :: Node -> String
formatNode (lbl, Nothing) = printf "  %s((%s))" lbl lbl
formatNode (lbl, Just tag) =
  let color = case tag of
        "actor"    -> "#fdf6e3"
        "director" -> "#eee8d5"
        "movie"    -> "#cb4b16"
        "book"     -> "#b58900"
        _          -> "#eee"
  in printf "  %s((\"%s (%s)\")):::t_%s" lbl lbl tag tag

formatEdge :: Edge -> String
formatEdge (u, v, Just lbl, True) = printf "  %s --|%s|--> %s" u lbl v
formatEdge (u, v, Just lbl, False) = printf "  %s ---|%s| %s" u lbl v
formatEdge (u, v, Nothing, True) = printf "  %s --> %s" u v
formatEdge (u, v, Nothing, False) = printf "  %s --- %s" u v

toMermaid :: Graph -> String
toMermaid (vs, es) = unlines $
  ["```mermaid", "graph TD"] ++
  map formatNode vs ++
  map formatEdge es ++
  ["classDef t_actor fill:#fdf6e3;", "classDef t_director fill:#eee8d5;", "classDef t_movie fill:#cb4b16;",
   "classDef t_book fill:#b58900;", "classDef t_default fill:#eee;", "```"]

writeMarkdownFile :: FilePath -> String -> IO ()
writeMarkdownFile path content = withFile path WriteMode $ \h -> hPutStr h content

-- Utils
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim s = case break (== delim) s of
  (a, _ : rest) -> a : splitOn delim rest
  (a, []) -> [a]
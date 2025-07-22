-- ZykovCore-topic.hs
-- Este módulo contiene la lógica principal para parsear y evaluar expresiones con etiquetas categóricas.

{-# LANGUAGE DeriveGeneric #-}

--module ZykovCoreTopic (Graph(..), Term(..), parseTerm) where

module ZykovCoreTopic (
    GraphExpr(..),
    evalGraphExpr,
    parseGraphExpr,
    parseExpressionWithCategories,
    NodeMeta(..)
) where

import Data.List (nub, sort, stripPrefix)
import Data.Char (isAlphaNum, isSpace)
import GHC.Generics (Generic)
import qualified Data.Map as M

-- Representación de una expresión gráfica extendida con categorías
-- Nodo puede tener una categoría opcional

-- Nodo con categoría opcional
data NodeMeta = NodeMeta
  { nodeId     :: String
  , category  :: Maybe String
  } deriving (Show, Eq, Ord, Generic)

-- Expresiones de grafos extendidas
data GraphExpr
  = Node NodeMeta
  | Union GraphExpr GraphExpr
  | Join GraphExpr GraphExpr
  deriving (Show, Eq)

-- Evaluación: devuelve nodos y aristas
-- Nodo = (id, categoría), Aristas = (source, target)
evalGraphExpr :: GraphExpr -> ([NodeMeta], [(String, String)])
evalGraphExpr (Node n) = ([n], [])
evalGraphExpr (Union a b) =
  let (ns1, es1) = evalGraphExpr a
      (ns2, es2) = evalGraphExpr b
  in (nub (ns1 ++ ns2), nub (es1 ++ es2))
evalGraphExpr (Join a b) =
  let (ns1, es1) = evalGraphExpr a
      (ns2, es2) = evalGraphExpr b
      newEdges = [(nodeId x, nodeId y) | x <- ns1, y <- ns2]
  in (nub (ns1 ++ ns2), nub (es1 ++ es2 ++ newEdges))

-- Parser minimalista con soporte para etiquetas
-- Ej: A :: category * (B + C)
parseGraphExpr :: String -> GraphExpr
parseGraphExpr s =
  case parseUnion (filter (not . isSpace) s) of
    (e, []) -> e
    (_, rem) -> error $ "Failed to parse. Remaining: " ++ rem

-- Parsing helpers
type Parser a = String -> (a, String)

parseUnion :: Parser GraphExpr
parseUnion s =
  let (a, rest) = parseJoin s in
  case rest of
    ('+':rs) -> let (b, rest') = parseUnion rs in (Union a b, rest')
    _        -> (a, rest)

parseJoin :: Parser GraphExpr
parseJoin s =
  let (a, rest) = parseAtom s in
  case rest of
    ('*':rs) -> let (b, rest') = parseJoin rs in (Join a b, rest')
    _        -> (a, rest)

parseAtom :: Parser GraphExpr
parseAtom ('(':s) =
  let (e, rest) = parseUnion s
  in case rest of
    (')':rs) -> (e, rs)
    _        -> error "Expected closing parenthesis"
parseAtom s =
  case span isValidChar s of
    (name, rest1) ->
      case stripPrefix "::::" rest1 of
        Just rest2 ->
          let (cat, rest3) = span isValidChar rest2 in
          case rest3 of
            ('*':rest4) ->
              let (expr, restFinal) = parseJoin rest4
              in (Join (Node (NodeMeta name (Just cat))) expr, restFinal)
            _ -> error "Expected '*' after category"
        Nothing -> (Node (NodeMeta name Nothing), rest1)


isValidChar :: Char -> Bool
isValidChar c = isAlphaNum c || c `elem` "_`"

-- Función exportada que produce tuplas (nodos, aristas)
parseExpressionWithCategories :: String -> ([(String, Maybe String)], [(String, String)])
parseExpressionWithCategories input =
  let expr = parseGraphExpr input
      (ns, es) = evalGraphExpr expr
  in (map (\n -> (nodeId n, category n)) ns, es)


--parseExpressionWithCategories :: String -> ([String], [(String, String)])
--parseExpressionWithCategories input =
--  let expr = parseGraphExpr input
--      (ns, es) = evalGraphExpr expr
--  in (map nodeId ns, es)


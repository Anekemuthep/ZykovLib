{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module ZykovLang3 (startServer) where

import Network.Wai.Middleware.Cors
import Data.List (nub, sort)
import Data.Char (isDigit)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import System.Process (callCommand)
import System.Directory (doesFileExist)

-- Servant Imports
import Servant
import Network.Wai
import Network.Wai.Handler.Warp (run)

-- ByteSrings imports
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI


-- Expression Tree Definition
data GraphExpr = Node String
               | Union GraphExpr GraphExpr
               | Join GraphExpr GraphExpr
               deriving (Show, Eq)

-- Evaluates a GraphExpr into nodes and edges
eval :: GraphExpr -> ([String], [(String, String)])
eval (Node x) = ([x], [])
eval (Union a b) =
  let (ns1, es1) = eval a
      (ns2, es2) = eval b
  in (nub (ns1 ++ ns2), nub (es1 ++ es2))
eval (Join a b) =
  let (ns1, es1) = eval a
      (ns2, es2) = eval b
      newEdges = [(x, y) | x <- ns1, y <- ns2]
  in (nub (ns1 ++ ns2), nub (es1 ++ es2 ++ newEdges))

-- Minimal parser from symbolic expressions
parseExpr :: String -> GraphExpr
parseExpr s = case parse s of
  (e, "") -> e
  _       -> error "Failed to parse expression"

parse :: String -> (GraphExpr, String)
parse s = parseUnion (filter (/= ' ') s)

parseUnion :: String -> (GraphExpr, String)
parseUnion s = let (a, rest) = parseJoin s in
  case rest of
    ('+':rs) -> let (b, rest') = parseUnion rs in (Union a b, rest')
    _        -> (a, rest)

parseJoin :: String -> (GraphExpr, String)
parseJoin s = let (a, rest) = parseAtom s in
  case rest of
    ('*':rs) -> let (b, rest') = parseJoin rs in (Join a b, rest')
    _        -> (a, rest)

parseAtom :: String -> (GraphExpr, String)
parseAtom ('(':s) =
  let (e, rest) = parseUnion s
  in case rest of
    (')':rs) -> (e, rs)
    _        -> error "Expected closing parenthesis"
parseAtom s =
  let (name, rest) = span (\c -> c /= '+' && c /= '*' && c /= ')') s
  in (Node name, rest)

-- Top-level interface
evalExpression :: String -> ([String], [(String, String)])
evalExpression expr = eval (parseExpr expr)

------------------------------------
-- Servant HTTP Backend
------------------------------------

-- Request input format
data EvalRequest = EvalRequest
  { expression :: String
  } deriving (Show, Generic)

instance FromJSON EvalRequest

-- Response output format
data EvalResponse = EvalResponse
  { nodes :: [String]
  , edges :: [(String, String)]
  } deriving (Show, Generic)

instance ToJSON EvalResponse

-- API Type
type EvalAPI = "eval" :> ReqBody '[JSON] EvalRequest :> Post '[JSON] EvalResponse

-- Server Handler
evalHandler :: EvalRequest -> Handler EvalResponse
evalHandler (EvalRequest expr) =
  let (ns, es) = evalExpression expr
  in return $ EvalResponse ns es

-- WAI App
server :: Server EvalAPI
server = evalHandler

app :: Application
app = serve (Proxy :: Proxy EvalAPI) server

-- Exported function
-- Exported function with fixed ByteString types
startServer :: IO ()
startServer = run 8080 $ cors (const $ Just policy) app
  where
    policy = simpleCorsResourcePolicy
      { corsRequestHeaders = [CI.mk (BS.pack "Content-Type")]
      , corsOrigins = Nothing  -- allows all origins
      , corsMethods = map BS.pack ["OPTIONS", "GET", "POST"]
      }


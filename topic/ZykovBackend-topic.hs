-- ZykovBackend-topic.hs
-- Backend del servidor que evalúa expresiones ZykovLang con etiquetas categóricas
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server
import Network.Wai.Middleware.Cors
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Data.Text as T

import ZykovCoreTopic

-- Tipos para entrada y salida JSON
data ExprRequest = ExprRequest
  { expression :: String
  } deriving (Show, Generic)

instance FromJSON ExprRequest

-- Nodo extendido con categoría opcional
data GraphResponse = GraphResponse
  { nodes :: [NodeOut]
  , edges :: [(String, String)]
  } deriving (Show, Generic)

instance ToJSON GraphResponse

-- Nodo serializable
data NodeOut = NodeOut
  { id       :: String
  , category :: Maybe String
  } deriving (Show, Generic)

instance ToJSON NodeOut

-- API
type API = "eval" :> ReqBody '[JSON] ExprRequest :> Post '[JSON] GraphResponse

server :: Server API
server = evalExpr
  where
    evalExpr (ExprRequest expr) = do
      let parsed = parseGraphExpr expr
      let (ns, es) = evalGraphExpr parsed
      let outNodes = map (\n -> NodeOut (nodeId n) (category n)) ns
      return $ GraphResponse outNodes es

app :: Application
app = simpleCors $ serve (Proxy :: Proxy API) server

main :: IO ()
main = do
  putStrLn "ZykovLangTopic backend corriendo en http://localhost:8080"
  run 8080 app

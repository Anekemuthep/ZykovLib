{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import ZykovLang3Topic (parseExpressionWithCategories)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import GHC.Generics

-- | JSON-friendly node format for frontend
data NodeResponse = NodeResponse
  { nodeId :: String
  , category :: Maybe String
  } deriving (Show, Generic)

instance ToJSON NodeResponse

-- | API Definition
type EvalAPI = "eval" :> ReqBody '[JSON] EvalRequest :> Post '[JSON] EvalResponse

-- | JSON Input
data EvalRequest = EvalRequest { expression :: String } deriving (Generic, Show)
instance FromJSON EvalRequest

-- | JSON Output
data EvalResponse = EvalResponse
  { nodes :: [NodeResponse]
  , edges :: [(String, String)]
  } deriving (Generic, Show)

instance ToJSON EvalResponse

-- | Server Logic
server :: Server EvalAPI
server = evalHandler
  where
    evalHandler (EvalRequest expr) = do
      let (vs, es) = parseExpressionWithCategories expr
          nodeResponses = map (\(v, mc) -> NodeResponse v mc) vs
      pure $ EvalResponse nodeResponses es

-- | Application
api :: Proxy EvalAPI
api = Proxy

app :: Application
--app = simpleCors $ serve api server
app = cors (const $ Just policy) $ serve api server
  where
    policy = simpleCorsResourcePolicy
      { corsRequestHeaders = ["Content-Type"]
      , corsOrigins = Just (["http://127.0.0.1:8081"], True)
      , corsMethods = ["GET", "POST", "OPTIONS"]
      }



main :: IO ()
main = do
  putStrLn "Starting Zykov Topic Backend at http://localhost:8080"
  run 8080 app

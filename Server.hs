-- Server.hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import ZykovLang3 (eval)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Set as Set

-- Graph response type

data GraphResponse = GraphResponse
  { nodes :: [String]
  , edges :: [(String, String)]
  } deriving (Generic, Show)

instance ToJSON GraphResponse

-- API type

type API = "eval" :> ReqBody '[JSON] EvalRequest :> Post '[JSON] GraphResponse

data EvalRequest = EvalRequest { expression :: Text } deriving (Generic, Show)
instance FromJSON EvalRequest

-- Server implementation

server :: Server API
server = evalHandler
  where
    evalHandler (EvalRequest expr) = do
      let (vs, es) = eval (unpack expr)
      return $ GraphResponse (Set.toList $ Set.fromList vs) (Set.toList $ Set.fromList es)

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
  putStrLn "Starting ZykovLang API on http://localhost:8080"
  run 8080 app

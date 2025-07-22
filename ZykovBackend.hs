{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ZykovBackend (startServer) where

import ZykovCore
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

-- API
type API = "eval" :> ReqBody '[JSON] ExprRequest :> Post '[JSON] GraphResponse

data ExprRequest = ExprRequest { expression :: String } deriving (Generic, Show)
instance FromJSON ExprRequest

data GraphResponse = GraphResponse { nodes :: [String], edges :: [[String]] } deriving (Generic, Show)
instance ToJSON GraphResponse

server :: Server API
server = evalHandler

evalHandler :: ExprRequest -> Handler GraphResponse
evalHandler (ExprRequest exprStr) =
  let (ns, es) = eval exprStr
  in return $ GraphResponse ns es

api :: Proxy API
api = Proxy

startServer :: IO ()
startServer = run 8080 (serve api server)

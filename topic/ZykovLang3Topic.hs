-- ZykovLang3Topic.hs
-- Parser y adaptador desde texto hacia nodos/aristas evaluados desde ZykovCoreTopic

{-# LANGUAGE OverloadedStrings #-}
--module ZykovLang3Topic where
module ZykovLang3Topic (parseExpressionWithCategories) where


import ZykovCoreTopic
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), ToJSON(..), object)
import qualified Data.ByteString.Lazy.Char8 as BL

-- Representación JSON de la respuesta
instance ToJSON NodeMeta where
  toJSON (NodeMeta nid mcat) =
    case mcat of
      Just c  -> object ["id" .= nid, "category" .= c]
      Nothing -> object ["id" .= nid]

data GraphResult = GraphResult
  { nodes :: [NodeMeta]
  , edges :: [(String, String)]
  }

instance ToJSON GraphResult where
  toJSON (GraphResult ns es) =
    object [ "nodes" .= ns
           , "edges" .= map (\(s,t) -> object ["source" .= s, "target" .= t]) es ]

-- Punto de entrada: toma un string y entrega JSON evaluado
processExpression :: String -> BL.ByteString
processExpression expr =
  let parsed = parseGraphExpr expr
      (ns, es) = evalGraphExpr parsed
  in Aeson.encode $ GraphResult ns es

--parseExpressionWithCategories :: String -> ([(String, Maybe String)], [(String, String)])
--parseExpressionWithCategories input =
--  let expr = parseGraphExpr input
--      (ns, es) = evalGraphExpr expr
--  in (map (\n -> (nodeId n, category n)) ns, es)


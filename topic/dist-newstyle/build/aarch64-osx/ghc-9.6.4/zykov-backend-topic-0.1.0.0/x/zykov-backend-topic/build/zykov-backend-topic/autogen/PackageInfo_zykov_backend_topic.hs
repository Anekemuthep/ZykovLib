{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_zykov_backend_topic (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "zykov_backend_topic"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "ZykovLang Topic Backend"
copyright :: String
copyright = ""
homepage :: String
homepage = ""

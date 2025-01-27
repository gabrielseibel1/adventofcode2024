{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_adventofcode2024 (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "adventofcode2024"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Solutions for https://adventofcode.com/2024"
copyright :: String
copyright = ""
homepage :: String
homepage = ""

{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_day01 (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "day01"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "aoc day 1 solution"
copyright :: String
copyright = ""
homepage :: String
homepage = ""

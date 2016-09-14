{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell, RankNTypes, TypeFamilies, MultiParamTypeClasses, RecordWildCards #-}

-- module Repa where
module Main where

import NaiveGame3
    
import Control.DeepSeq
import Control.Exception (evaluate)
import GHC.Generics (Generic)
import Linear
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB
import Control.Monad.State.Strict
import Data.Bits
import Data.Word
import Data.Int
import Criterion
import Criterion.Types
import Criterion.Main
import System.Clock

import Data.Vector.Unboxed.Base (Unbox)
import Data.Vector.Unboxed.Deriving

import qualified Data.Array.Repa as R
    

main = undefined

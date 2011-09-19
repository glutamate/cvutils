{-# LANGUAGE ViewPatterns #-}

module Edge where

import Codec.Image.DevIL
import Data.Array.Unboxed
import Math.Probably.MCMC
import Math.Probably.Sampler
import Math.Probably.FoldingStats
import Control.Monad.State.Strict 
import System.Cmd
import System.Environment
import Data.Array.IO
import System.IO
import Baysig.Estimate.RTS
import Data.Array.Unboxed
import Numeric.LinearAlgebra hiding (find)
import qualified Math.Probably.PDF as PDF
import Data.List
import Data.Maybe
import Control.Applicative
import Data.Ord
import Graphics.Triangulation.Triangulation

loadPoints :: IO [(Double,Double)]
loadPoints = fmap read $ readFile "edge.dat"


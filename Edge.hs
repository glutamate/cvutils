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
import Data.List hiding (map)
import Data.Maybe
import Control.Applicative
import Data.Ord
import Graphics.Triangulation.Triangulation
import qualified Data.Vector as V
import Graphics.Formats.Collada.Vector2D3D

pairToV :: (Double,Double) -> V2
pairToV (x,y) = V (realToFrac x) (realToFrac y)

type Polygon = V.Vector V2

toPoly :: [(Double,Double)] -> Polygon
toPoly pts = V.fromList $ map pairToV pts

inside :: Polygon -> (Double,Double)  -> Bool
inside poly xy = pointInside (pairToV xy) poly

loadPoints :: IO [(Double,Double)]
loadPoints = fmap read $ readFile "edge.dat"


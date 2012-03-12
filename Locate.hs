{-# LANGUAGE ViewPatterns #-}

module Main where

import Codec.Image.DevIL
import Data.Array.Unboxed
import Math.Probably.MCMC
import Math.Probably.RandIO
import Math.Probably.Sampler
import Math.Probably.FoldingStats
import Control.Monad.State.Strict 
import System.Cmd
import System.Environment
import Data.Array.IO
import System.IO

import Data.Array.Unboxed
import Numeric.LinearAlgebra hiding (find)
import qualified Math.Probably.PDF as PDF
import Data.List
import Data.Maybe
import Control.Applicative
import Data.Ord
 
import Data.IORef
import System.IO.Unsafe

import CVUtils
import Edge


posteriorV :: Image -> Image -> (Int, Int) -> Vector R -> R
posteriorV bgim im (cx,cy) v = 
   uniformLogPdf 0 100.0 len1 +
   uniformLogPdf 0 1.0 ecc +
   uniformLogPdf 0 10000.0 noise +
   uniformLogPdf 0 1500.0 px +
   uniformLogPdf 0 1500.0 py +
   sum [ f x y chan | 
                   x <- [ (cx::Int) -20.. cx +20],
                   y <- [ cy -20.. cy +20], 
                   chan <- [0..2]]

  where px = v @> 0
        py = v @> 1
        noise = v @> 2
        len1 = v@> 3
        rot = v@> 4
        ecc = v@> 5
        red = v@> 6
        green = v@> 7
        blue = v@> 8
        noiseInside = v@> 9
        len2 = v@> 10
        f1x = px+(len1*ecc)*cos rot
        f1y = py+(len1*ecc)*sin rot
        f2x = px-(len1*ecc)*cos rot
        f2y = py-(len1*ecc)*sin rot
        f1x2 = px+(len2*ecc)*cos rot
        f1y2 = py+(len2*ecc)*sin rot
        f2x2 = px-(len2*ecc)*cos rot
        f2y2 = py-(len2*ecc)*sin rot
        colVec = fromList [red,green,blue]
        f :: Int -> Int -> Int -> R
        f x y ch 
         = if dist  f1x f1y   x  y  + dist  f2x f2y   x  y  < 2 * len1
              then gaussR noiseInside (colVec@>ch) $ im!(y,x,ch)
              else if dist  f1x2 f1y2   x  y  + dist  f2x2 f2y2   x  y  < 2 * len2
                      then gaussW8 noise ((bgim!(y,x,ch))`div` 2) $ im!(y,x,ch)
                      else gaussW8 noise (bgim!(y,x,ch)) $ im!(y,x,ch)


main = do
     ilInit
     bgnm : fvid :_ <- getArgs
     bgIm <-readImage bgnm
     system $ "~/cvutils/extract "++fvid++" 3200"
     frame0 <-readImage "extract.png"
     let x = 1092
         y = 518
         rot = -5.12
         posterior = posteriorV bgIm frame0 (round x,round y)
         postAndV v = (posteriorV bgIm frame0 (round x,round y) $ fromList v, v)
         
         initialsV = fromList [x,y,218, 4.5, rot, 0.9, 0.1,0.1,0.1, 218, 6]
     print [frame0!(round y,round x,c) | c <- [0..2]]
     print [frame0!(round y+1,round x,c) | c <- [0..2]]
     print [frame0!(round y,round x+1,c) | c <- [0..2]]
     runRIO $ do
         iniampar <- sample $ initialAdaMet 300 1.5e-4 posterior initialsV
         AMPar v _ _ _ _ _ _ <- runAndDiscard 50000 (show . ampPar) iniampar $ adaMet False posterior
         lift $ print initialsV
         lift $ print v
module Main where

import Codec.Image.DevIL
import Data.Array.Unboxed
import Math.Probably.MCMC
import Math.Probably.Sampler
import Math.Probably.FoldingStats
import Control.Monad.State.Strict 
import System.Cmd
import System.Environment
import Data.Array.Unboxed

type R = Double

type Pos = (R,R)
type Image = UArray (Int, Int, Int) Word8

main = do
     ilInit
     f1 : f2 :_ <- getArgs
     im1 <- readImage f1
     im2 <- readImage f2
     let ((lox, loy, loch), (hix, hiy, hich)) = bounds im1
     let pxVal x y ch = im1!(x,y,ch)
     let im3 = array (bounds im1) 
                     [ ((x,y,ch),pxVal x y ch ) 
                                | x<- [lox..hix], y<- [lox..hix], ch <- [loch..hich]]
     writeImage "mixed" im3
     return ()


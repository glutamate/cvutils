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
halfz = 50

mixit middle rhz y = (1-0.5*realToFrac y/rhz+0.5/rhz*(realToFrac middle-rhz))

main = do
     ilInit
     f1 : f2 :_ <- getArgs
     im1 <- readImage f1
     im2 <- readImage f2
    
     let ((lox, loy, loch), (hix, hiy, hich)) = bounds im1

     let middle = hiy `div` 2
         rhz = realToFrac halfz
     --print [mixit middle rhz y  | y<- [middle - halfz..middle + halfz]]
     let pxVal x y ch = case () of
          _ | (y< middle - halfz) -> im1!(x,y,ch)
          _ | (y>= middle + halfz) -> im2!(x,y,ch)
          _  -> realToW8 $ mixit middle rhz y * (word8ToR $ im1!(x,y,ch)) + (1-mixit middle rhz y) * (word8ToR $ im2!(x,y,ch))
     let im3 = array (bounds im1) 
                     [ ((x,y,ch),pxVal x y ch ) 
                                | x<- [lox..hix], y<- [loy..hiy], ch <- [loch..hich]]
     writeImage "mixed.png" im3
     return ()

word8ToR :: Word8 -> Double
word8ToR = (/256) . realToFrac . fromEnum
realToW8 :: Double -> Word8
realToW8 = toEnum . round . (bound 0 255). (*256)  

bound lo hi x = max lo $ min hi x
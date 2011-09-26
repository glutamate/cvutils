{-# LANGUAGE ViewPatterns #-}

module CVUtils where

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

import Data.IORef
import System.IO.Unsafe

type R = Double

type Pos = (R,R)
type Image = UArray (Int, Int, Int) Word8
type MImage = IOUArray (Int, Int, Int) Word8


{-track1 :: Image -> [Obj] -> Image -> Sampler Obj
track1 bgIm objs im = 
   samplingImportanceResampling $ particleLike bgIm im objs -}


type Square = (Pos,Pos)

within ((lox,loy), (hix,hiy)) ox oy
       =    ox > min lox hix
         && ox < max lox hix
         && oy > min loy hiy
         && oy < max loy hiy


data Obj = 
  Obj {vellen :: !R,
       sideDisp :: !R,
       posx :: !R, 
       posy :: !R, 
       rot :: !R  } deriving Show

fileroot = reverse . takeWhile (/='/') . reverse . takeWhile (/='.')

         
print2 x y = putStrLn $ x ++show y

smallest ws = foldl1' (min) $ map snd ws
largest ws = map snd $ sortBy (comparing snd) ws
sumWeights ws = sum $ map (exp . (subtr (smallest ws)) . snd) ws
cummWeightedSamples ws= 
   let least = smallest ws
   in scanl (\(_,csum) (x,w) -> (x,csum+exp (w-least))) (undefined,0) ws

dropLosers ws = 
  let topW = foldl1' (max) $ map snd ws
      p (x,w) = w>topW-100
  in sortBy (comparing snd) $ filter p ws

subtr x y = y - x


maxOn f xs = ceiling $ foldl1' max $ map f xs
minOn f xs = floor $ foldl1' min $ map f xs
 
data StaticParams = SP {noise :: !R,
                        wlength :: !R,
                        eccentric :: !R } deriving Show


dist :: R -> R -> Int ->Int -> R 
dist  cx cy   x y  = let dx = cx-(realToFrac x+0.5)
                         dy = cy-(realToFrac y+0.5)
                      in sqrt(dx*dx + dy*dy)

dist' :: R -> R -> Int ->Int -> R 
dist'  cx cy   x y  = let dx = cx-(realToFrac x+0.5)
                          dy = cy-(realToFrac y+0.5)
                      in dx*dx + dy*dy


gaussW8 :: R -> Word8 -> Word8 -> R
gaussW8 tau muw8 = lpdf . word8ToR  
   where lpdf x = log (sqrt (tau/2.0*pi)) + (0.0-((x-mu)*(x-mu)*tau))
         mu = word8ToR  muw8
gaussR :: R-> R -> Word8 -> R
gaussR tau mu = lpdf . word8ToR  
   where lpdf :: R -> R
         lpdf x = log (sqrt (tau/2.0*pi)) + (0.0-((x-mu)*(x-mu)*tau))

--no-noise versions
gaussW8nn :: R -> Word8 -> Word8 -> R
gaussW8nn tau muw8 = lpdf . word8ToR  
   where lpdf x = let y = x - mu in negate $ y*y
         mu = word8ToR  muw8
gaussRnn :: R-> R -> Word8 -> R
gaussRnn tau mu = lpdf . word8ToR  
   where lpdf :: R -> R
         lpdf x = let y = x - mu in negate $ y*y


word8ToR :: Word8 -> Double
word8ToR = (/256) . realToFrac 
realToW8 :: Double -> Word8
realToW8 =  round. (*256) 

markObjsOnImage :: [Obj] -> Image -> IO (Image)
markObjsOnImage objs im = do
    mutIm <- thaw im
    forM_ objs $ \(Obj _ _ cx cy _) -> do
          writeArray mutIm (round cy, round cx, 2) 255
          writeArray mutIm (round cy, round cx, 0) 0
          writeArray mutIm (round cy, round cx, 1) 0

    freeze (mutIm::MImage)
    
--updateBgIm :: Image -> 




         
mkRed arr cx cy = do 
          writeArray arr ( cy,
                             cx, 0) 255
--          writeArray arr ( cy,
--                             cx, 1) 0
--          writeArray arr ( cy,
--                             cx, 2) 0



posteriorV :: Image -> Image -> (Int, Int) -> Vector R -> R
posteriorV bgim im (cx,cy) v = 
   uniformLogPdf 0 100.0 len +
   uniformLogPdf 0 1.0 ecc +
   uniformLogPdf 0 10000.0 noise +
   uniformLogPdf 0 1500.0 px +
   uniformLogPdf 0 1500.0 py +
   sum [ f x y chan | 
                   x <- [ (cx::Int) -50.. cx +50],
                   y <- [ cy -50.. cy +50], 
                   chan <- [0..2]]

  where px = v @> 0
        py = v @> 1
        noise = v @> 2
        len = v@> 3
        rot = v@> 4
        ecc = v@> 5
        f1x = px+(len*ecc)*cos rot
        f1y = py+(len*ecc)*sin rot
        f2x = px-(len*ecc)*cos rot
        f2y = py-(len*ecc)*sin rot
        f :: Int -> Int -> Int -> R
        f x y ch 
         = if dist  f1x f1y   x  y  + dist  f2x f2y   x  y  < 2 * len
              then gaussR noise 0 $ im!(y,x,ch)
              else gaussW8 noise (bgim!(y,x,ch)) $ im!(y,x,ch)

markEllipse :: StaticParams -> Obj -> Image -> IO (Image)
markEllipse sp@(SP noise len ecc) (Obj _ _ px py rot) im = do
    mutIm <- thaw im
    
    let cx = round px
    let cy = round py
        f1x = px+(len*ecc)*cos rot
        f1y = py+(len*ecc)*sin rot
        f2x = px-(len*ecc)*cos rot
        f2y = py-(len*ecc)*sin rot
    let f x y = do -- print2 "f at" (x,y,dist (f1x,f1y) (x, y) + dist (f2x,f2y) (x, y) )
                   when (dist  f1x f1y   x  y  + dist  f2x f2y   x  y  < 2 * len) $ do
                      --print2 "red at " (x,y) 
                      mkRed mutIm x y
                      return ()
    sequence_ [ f x y  | 
                   x <- [ (cx::Int) -50.. cx +50],
                   y <- [ cy -50.. cy +50]]
    freeze (mutIm::MImage)



uniformLogPdf :: R -> R-> R-> R
uniformLogPdf from to = \x-> if x>=from && x <=to
                               then log $ realToFrac $ 1/(to-from )
                               else -1e20




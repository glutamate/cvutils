module CVUtils where

import Codec.Image.DevIL
import Data.Array.Unboxed

type R = Double

type Pos = (R,R)
type Image = UArray (Int, Int, Int) Word8

data Obj = Obj {pos :: Pos }

pixelNoise :: R
pixelNoise = 1.0
radius = 10.0
radiusi = round radius
maxOn f xs = ceiling $ foldr1 max $ map f xs
minOn f xs = floor $ foldr1 min $ map f xs

particleLike :: Image -> Image -> [Obj] -> [R]
particleLike bgim im objs = map pL objs where
  xs = [minOn (fst . pos) objs - radiusi..maxOn (fst . pos) objs+radiusi ] -- calc region of interest from all objs
  ys = [minOn (snd . pos) objs-radiusi..maxOn (snd . pos) objs+radiusi]
  pL (Obj (cx,cy)) = sum [ f cx cy x y chan | x <- xs, y <- ys, chan <- [0..2]]
  f cx cy x y ch 
    = if sqrt((cx-realToFrac x)^2+(cy-realToFrac y)^2) < radius
         then gaussR 0 $ im!(x,y,ch)
         else gaussW8 (bgim!(x,y,ch)) $ im!(x,y,ch)

gaussW8 :: Word8 -> Word8 -> R
gaussW8 muw8 = lpdf . word8ToR  
   where lpdf x = log (sqrt (tau/2.0*pi)) + (0.0-((x-mu)*(x-mu)*tau))
         tau = pixelNoise
         mu = word8ToR  muw8
         
gaussR :: R -> Word8 -> R
gaussR mu = lpdf . word8ToR  
   where lpdf :: R -> R
         lpdf x = log (sqrt (tau/2.0*pi)) + (0.0-((x-mu)*(x-mu)*tau))
         tau = pixelNoise

word8ToR = (/256) . realToFrac . fromEnum
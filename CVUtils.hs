module Main where

import Codec.Image.DevIL
import Data.Array.Unboxed
import Math.Probably.MCMC
import Math.Probably.Sampler
import Math.Probably.FoldingStats
import Control.Monad.State.Strict 
import System.Cmd
import System.Environment
import Data.Array.IO
import Data.Array.Unboxed

type R = Double

type Pos = (R,R)
type Image = UArray (Int, Int, Int) Word8
type MImage = IOUArray (Int, Int, Int) Word8

data Obj = Obj {pos :: Pos } deriving Show

track1 :: Image -> [Obj] -> Image -> Sampler Obj
track1 bgIm objs im = 
   samplingImportanceResampling $ particleLike bgIm im objs

diffuse :: Obj -> Sampler Obj
diffuse (Obj (x,y)) = do
        dx <- gaussD 0 3
        dy <- gaussD 0 3
        return $ Obj ((x+dx),y+dy)

track :: Image -> String -> Int -> (R,R) -> StateT Seed IO [(R,R)]
track bgIm vidfnm nframes x0y0 = do
  go bgIm (replicate nparticles $ Obj x0y0 ) [1..nframes] where
    go bgIm objs [] = return []
    go bgIm objs (i:is) = do
           lift $ system $"~/cvutils/extract "++vidfnm++" "++show i 
           frame <- lift $ readImage "extract.png"
           diffObjs <- sample $ mapM diffuse objs
           let wparticles = particleLike bgIm frame diffObjs
           let tracker = samplingImportanceResampling wparticles
           lift $ mapM_ print wparticles
           nextObjs <- sample $ sequence 
                              $ replicate nparticles tracker 
           markedIm <- lift $ markObjsOnImage nextObjs frame
           lift $ writeImage ("frame"++show i++".png") markedIm
           rest <- go bgIm nextObjs is
           return $ (runStat (both (before meanF (fst .pos)) 
                                   (before meanF (snd .pos)))
                              nextObjs):rest
          

{-trackVideo :: Image -> [Image] -> Obj -> Sampler [[Obj]]
trackVideo bgIm ims' o0 = go (replicate nparticles o0) ims' where
  go objs [] = return []
  go objs (im:ims) = do
    diffObjs <- mapM diffuse objs
    let wparticles = particleLike bgIm im diffObjs
    let tracker = samplingImportanceResampling wparticles
    nextObjs <- sequence $ replicate nparticles $ tracker 
    rest <- go nextObjs ims
    return $ nextObjs:rest -}

sample :: Sampler a -> StateT Seed IO a
sample (Sam sam) = do
       sd <- get
       let (x,nsd) = sam sd
       put nsd
       return x

pixelNoise :: R
pixelNoise = 1.0
radius = 10.0
nparticles = 100

radiusi = round radius
maxOn f xs = ceiling $ foldr1 max $ map f xs
minOn f xs = floor $ foldr1 min $ map f xs
 
particleLike :: Image -> Image -> [Obj] -> [(Obj,R)]
particleLike bgim im objs = map pL objs where
  xs = [minOn (fst . pos) objs - radiusi..maxOn (fst . pos) objs+radiusi ] -- calc region of interest from all objs
  ys = [minOn (snd . pos) objs-radiusi..maxOn (snd . pos) objs+radiusi]
  pL o@(Obj (cx,cy)) 
     = (o,sum [ f cx cy x y chan | 
                   x <- xs, 
                   y <- ys, 
                   chan <- [0..2]])
  f cx cy x y ch 
    = if sqrt((cx-realToFrac x)^2+(cy-realToFrac y)^2) < radius
         then gaussR 0 $ im!(y,x,ch)
         else gaussW8 (bgim!(y,x,ch)) $ im!(y,x,ch)

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

word8ToR :: Word8 -> Double
word8ToR = (/256) . realToFrac . fromEnum
realToW8 :: Double -> Word8
realToW8 = toEnum . round. (*256) 

markObjsOnImage :: [Obj] -> Image -> IO (Image)
markObjsOnImage objs im = do
    mutIm <- thaw im
    forM_ objs $ \(Obj (cx,cy)) -> do
          writeArray mutIm (toEnum . round $ cy,
                            toEnum . round $ cx, 0) 255
          writeArray mutIm (toEnum . round $ cy,
                            toEnum . round $ cx, 1) 0
          writeArray mutIm (toEnum . round $ cy,
                            toEnum . round $ cx, 2) 0
    freeze (mutIm::MImage)
    
         

main = do
     ilInit
     bgnm : fvid : pt0 : _ <- getArgs
     system $ "~/cvutils/extract "++fvid++" 1"
     bgIm <-readImage bgnm
     frame0 <-readImage "extract.png"
     let (x,y) = read pt0
     marked <- markObjsOnImage [Obj (x,y), 
                                Obj (x+1,y), 
                                Obj (x,y+1), 
                                Obj (x+1,y+1)] frame0
     writeImage "marked.png" marked
     seed <- getSeedIO
     poss <- evalStateT (track bgIm fvid 100 (x,y)) seed
     mapM_ print (poss :: [(R,R)])
     return ()

-- initial on wl0: (956,641)

--rm marked.png && track mixed.png ~/Dropbox/woodlice/wl0.avi '(956,641)' && eog marked.png
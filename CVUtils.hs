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
import Baysig.Estimate.RTS
import Data.Array.Unboxed
import Numeric.LinearAlgebra 
import qualified Math.Probably.PDF as PDF
import Data.List
import Data.Ord


type R = Double

type Pos = (R,R)
type Image = UArray (Int, Int, Int) Word8
type MImage = IOUArray (Int, Int, Int) Word8

data Obj = Obj {pos :: Pos } deriving Show

{-track1 :: Image -> [Obj] -> Image -> Sampler Obj
track1 bgIm objs im = 
   samplingImportanceResampling $ particleLike bgIm im objs -}

diffuse :: Obj -> Sampler Obj
diffuse (Obj (x,y)) = do
        dx <- gaussD 0 1
        dy <- gaussD 0 1
        return $ Obj ((x+dx),y+dy)

track :: R-> R-> Image -> String -> Int -> (R,R) -> StateT Seed IO [(R,R)]
track noise radius bgIm vidfnm nframes x0y0 = do
  go bgIm (replicate nparticles $ Obj x0y0 ) [1..nframes] where
    go bgIm objs [] = return []
    go bgIm objs (i:is) = do
           lift $ system $"~/cvutils/extract "++vidfnm++" "++show i 
           frame <- lift $ readImage "extract.png"
           diffObjs <- sample $ mapM diffuse objs
           let wparticles = dropLosers $ particleLike noise radius bgIm frame diffObjs
           let tracker = samplingImportanceResampling wparticles
           --lift $ mapM_ print wparticles
           lift $ print2 "smallest: "  (smallest wparticles)
           --lift $ print2 "largest: "  (largest wparticles)
           lift $ print2 "sumws: "  (sumWeights wparticles)
           nextObjs <- sample $ sequence 
                              $ replicate nparticles tracker 
           markedIm <- lift $ markObjsOnImage nextObjs frame
           lift $ writeImage ("frame"++show i++".png") markedIm
           rest <- go bgIm nextObjs is
           return $ (runStat (both (before meanF (fst .pos)) 
                                   (before meanF (snd .pos)))
                              nextObjs):rest
          
print2 x y = putStrLn $ x ++show y

smallest ws = foldl' (\acc (v,logpdf) -> min acc logpdf) (snd $ head ws) ws
largest ws = map snd $ sortBy (comparing snd) ws
sumWeights ws = sum $ map (exp . (subtr (smallest ws)) . snd) ws
cummWeightedSamples ws= scanl (\(_,csum) (x,w) -> (x,csum+exp (w-smallest ws))) (undefined,0) $ sortBy (comparing snd) ws

dropLosers ws = 
  let srt = sortBy (comparing snd) ws
      topW = snd $ last srt
      p (x,w) = w>topW-100
  in filter p srt

subtr x y = y - x

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

{-sample :: Sampler a -> StateT Seed IO a
sample (Sam sam) = do
       sd <- get
       let (x,nsd) = sam sd
       put nsd
       return x -}

nparticles = 1000

maxOn f xs = ceiling $ foldr1 max $ map f xs
minOn f xs = floor $ foldr1 min $ map f xs
 
data StaticParams = SP {noise :: R,
                        length :: R,
                        eccentric :: R }

particleLike :: R-> R-> Image -> Image -> [Obj] -> [(Obj,R)]
particleLike noise radius bgim im objs = map pL objs where
  radiusi = round radius + 5
  xs = [minOn (fst . pos) objs - radiusi..maxOn (fst . pos) objs+radiusi ] -- calc region of interest from all objs
  ys = [minOn (snd . pos) objs-radiusi..maxOn (snd . pos) objs+radiusi]
  pL o@(Obj (cx,cy)) 
     = (o, sum [ f cx cy x y chan | 
                   x <- xs, 
                   y <- ys, 
                   chan <- [0..2]])
  f cx cy x y ch 
    = if sqrt((cx-realToFrac x)^2+(cy-realToFrac y)^2) < radius
         then gaussR noise 0 $ im!(y,x,ch)
         else gaussW8 noise (bgim!(y,x,ch)) $ im!(y,x,ch)

dist :: (R, R) -> (Int,Int) -> R 
dist (cx,cy) (x,y) = sqrt((cx-realToFrac x)^2+(cy-realToFrac y)^2)

gaussW8 :: R -> Word8 -> Word8 -> R
gaussW8 tau muw8 = lpdf . word8ToR  
   where lpdf x = log (sqrt (tau/2.0*pi)) + (0.0-((x-mu)*(x-mu)*tau))
         mu = word8ToR  muw8
         
gaussR :: R-> R -> Word8 -> R
gaussR tau mu = lpdf . word8ToR  
   where lpdf :: R -> R
         lpdf x = log (sqrt (tau/2.0*pi)) + (0.0-((x-mu)*(x-mu)*tau))

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
     let initialsV = fromList [x,y,218, 2.95]
         posterior = posteriorV bgIm frame0 (round x,round y)
         postAndV v = (posteriorV bgIm frame0 (round x,round y) $ fromList v, v)
     print $ postAndV [x,y,218.0, 2.8] 
     print $ postAndV [x,y,218.0, 2.9]
     print $ postAndV [x,y,218.0, 3.0]
     print $ postAndV [x,y,218.0, 3.1] 
     print $ postAndV [x,y,218.0, 3.2]
     runRIO $ do
         --iniampar <- sample $ initialAdaMet 200 5e-4 posterior initialsV
         --AMPar v _ _ _ <- runAndDiscard 2000 (show . ampPar) iniampar $ adaMet False posterior
         --lift $ print v
--         track (v@> 2) (v @> 3) bgIm fvid 3 (v@>0,v@>1)
         track (218) (3) bgIm fvid 300 (x,y)
     
     return () 

posteriorV bgim im (cx,cy) v = 
   uniformLogPdf 0 100.0 radius +
   uniformLogPdf 0 10000.0 noise +
   uniformLogPdf 0 1500.0 px +
   uniformLogPdf 0 1500.0 py +
   sum [ f px py x y chan | 
                   x <- [ cx -50.. cx +50],
                   y <- [ cy -50.. cy +50], 
                   chan <- [0..2]]

  where px = v @> 0
        py = v @> 1
        noise = v @> 2
        radius = v@> 3
        f ox oy x y ch 
          = if sqrt((ox-realToFrac x)^2+(oy-realToFrac y)^2) < radius
               then gaussR noise 0 $ im!(y,x,ch)
               else gaussW8 noise (bgim!(y,x,ch)) $ im!(y,x,ch)

uniformLogPdf :: R -> R-> R-> R
uniformLogPdf from to = \x-> if x>=from && x <=to
                               then log $ realToFrac $ 1/(to-from )
                               else -1e20



-- initial on wl0: (956,641)

--rm marked.png && track mixed.png ~/Dropbox/woodlice/wl0.avi '(956,641)' && eog marked.png
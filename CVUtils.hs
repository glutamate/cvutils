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

data Obj = Obj {pos :: Pos, rot :: R  } deriving Show

{-track1 :: Image -> [Obj] -> Image -> Sampler Obj
track1 bgIm objs im = 
   samplingImportanceResampling $ particleLike bgIm im objs -}

diffuse :: Obj -> Sampler Obj
diffuse (Obj (x,y) rot) = do
        nx <- gaussD x 1
        ny <- gaussD y 1
        nrot <- gaussD rot 0.1
        return $ Obj (nx,ny) nrot

track :: StaticParams -> Image -> String -> Int -> Obj -> StateT Seed IO [(R,R)]
track sp bgIm vidfnm nframes obj0 = do
  go bgIm (replicate nparticles obj0) [1..nframes] where
    go bgIm objs [] = return []
    go bgIm objs (i:is) = do
           lift $ system $"~/cvutils/extract "++vidfnm++" "++show i 
           frame <- lift $ readImage "extract.png"
           diffObjs <- sample $ mapM diffuse objs
           let wparticles = dropLosers $ particleLike sp bgIm frame diffObjs
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
                        eccentric :: R } deriving Show

particleLike :: StaticParams -> Image -> Image -> [Obj] -> [(Obj,R)]
particleLike sp@(SP noise len ecc) bgim im objs = map pL objs where
  radiusi = round len + 5
  xs = [minOn (fst . pos) objs - radiusi..maxOn (fst . pos) objs+radiusi ] -- calc region of interest from all objs
  ys = [minOn (snd . pos) objs-radiusi..maxOn (snd . pos) objs+radiusi]
  pL o@(Obj (cx,cy) rot) = 
    let f1x = cx+(len/ecc)*cos rot
        f1y = cx+(len/ecc)*sin rot
        f2x = cx-(len/ecc)*cos rot
        f2y = cx-(len/ecc)*sin rot
        f rot cx cy x y ch 
         = if dist (f1x,f1y) (x, y) + dist (f2x,f2y) (x, y) < 2 * len
              then gaussR noise 0 $ im!(y,x,ch)
              else gaussW8 noise (bgim!(y,x,ch)) $ im!(y,x,ch)
    in (o, sum [ f rot cx cy x y chan | 
                   x <- xs, 
                   y <- ys, 
                   chan <- [0..2]])

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
    forM_ objs $ \(Obj (cx,cy) _) -> mkRed mutIm (round cx) (round cy)
    freeze (mutIm::MImage)
    
         
mkRed arr cx cy = do 
          writeArray arr ( cy,
                             cx, 0) 255
          writeArray arr ( cy,
                             cx, 1) 0
          writeArray arr ( cy,
                             cx, 2) 0

main = do
     ilInit
     bgnm : fvid : pt0 : _ <- getArgs
     system $ "~/cvutils/extract "++fvid++" 1"
     bgIm <-readImage bgnm
     frame0 <-readImage "extract.png"
     let (x,y) = read pt0
     {-marked <- markObjsOnImage [Obj (x,y) 0, 
                                Obj (x+1,y) 0, 
                                Obj (x,y+1) 0, 
                                Obj (x+1,y+1) 0] frame0
     writeImage "marked.png" marked -}
     let initialsV = fromList [x,y,218, 8, negate (pi/6), 0.4]
         posterior = posteriorV bgIm frame0 (round x,round y)
         postAndV v = (posteriorV bgIm frame0 (round x,round y) $ fromList v, v)
         sp = (SP 218 20 0.8)
     ellim <- markEllipse sp  (Obj (x,y) $ negate (pi/6)) frame0
     writeImage "markell.png" ellim
     print $ postAndV [x,y,218, 8, negate (pi/6), 0.8]
     print $ postAndV [x,y,218, 8, 0, 0.8]
{-
     runRIO $ do
         iniampar <- sample $ initialAdaMet 100 5e-4 posterior initialsV
         AMPar v _ _ _ <- runAndDiscard 5000 (show . ampPar) iniampar $ adaMet False posterior
         lift $ print v
--         track (v@> 2) (v @> 3) bgIm fvid 3 (v@>0,v@>1)
--         track (SP 218 6 0.8) bgIm fvid 300 $ Obj (x,y) 0
     -}
     return () 

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
        f1x = px+(len/ecc)*cos rot
        f1y = py+(len/ecc)*sin rot
        f2x = px-(len/ecc)*cos rot
        f2y = py-(len/ecc)*sin rot
        f :: Int -> Int -> Int -> R
        f x y ch 
         = if dist (f1x,f1y) (x, y) + dist (f2x,f2y) (x, y) < 2 * len
              then gaussR noise 0 $ im!(y,x,ch)
              else gaussW8 noise (bgim!(y,x,ch)) $ im!(y,x,ch)

markEllipse :: StaticParams -> Obj -> Image -> IO (Image)
markEllipse sp@(SP noise len ecc) (Obj (px, py) rot) im = do
    mutIm <- thaw im
    
    let cx = round px
    let cy = round py
        f1x = px+(len*ecc)*cos rot
        f1y = py+(len*ecc)*sin rot
        f2x = px-(len*ecc)*cos rot
        f2y = py-(len*ecc)*sin rot
    print sp
    print (f1x, f1y)
    print (f2x, f2y)
    print $ dist (f1x,f1y) (cx, cy)
    let f x y = do -- print2 "f at" (x,y,dist (f1x,f1y) (x, y) + dist (f2x,f2y) (x, y) )
                   when (dist (f1x,f1y) (x, y) + dist (f2x,f2y) (x, y) < 2 * len) $ do
                      print2 "red at " (x,y) 
                      mkRed mutIm x y
    sequence_ [ f x y  | 
                   x <- [ (cx::Int) -50.. cx +50],
                   y <- [ cy -50.. cy +50]]
    freeze (mutIm::MImage)



uniformLogPdf :: R -> R-> R-> R
uniformLogPdf from to = \x-> if x>=from && x <=to
                               then log $ realToFrac $ 1/(to-from )
                               else -1e20



-- initial on wl0: (956,641)

--rm marked.png && track mixed.png ~/Dropbox/woodlice/wl0.avi '(956,641)' && eog marked.png
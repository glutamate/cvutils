{-# LANGUAGE ViewPatterns #-}

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
import System.IO
import Baysig.Estimate.RTS
import Data.Array.Unboxed
import Numeric.LinearAlgebra hiding (find)
import qualified Math.Probably.PDF as PDF
import Data.List
import Data.Maybe
import Control.Applicative
import Data.Ord


type R = Double


type Pos = (R,R)
type Image = UArray (Int, Int, Int) Word8
type MImage = IOUArray (Int, Int, Int) Word8

data Obj = 
  Obj {velx :: !R,
       vely :: !R,
       vrot :: !R,
       posx :: !R, 
       posy :: !R, 
       rot :: !R  } deriving Show

{-track1 :: Image -> [Obj] -> Image -> Sampler Obj
track1 bgIm objs im = 
   samplingImportanceResampling $ particleLike bgIm im objs -}

sdv = 3
sdvrot = 0.3
evolve :: Obj -> Sampler (Obj,Obj)
evolve o@(Obj vx vy vrot x y rot) = do
        nvx <- gaussD vx sdv
        nvy <- gaussD vy sdv
        nvrot <- gaussD vrot sdvrot
        return $ (o,Obj nvx nvy nvrot (x+nvx) (y+nvy) (rot+nvrot))

type Square = (Pos,Pos)

within ((lox,loy), (hix,hiy)) ox oy
       =    ox > min lox hix
         && ox < max lox hix
         && oy > min loy hiy
         && oy < max loy hiy

invisible :: [((Int,Int),(Int,Int))]

invisible = [ ((963,626), 
               (999,663))]

visible :: Int -> Int -> Bool
visible x y  = not $ any (\sqr -> within sqr  x  y) invisible

objCentreInvisible :: Obj -> Bool
objCentreInvisible (Obj _ _ _ cx cy _) = not $ visible (round cx) (round cy)

nogo::  [Square]
nogo = 
  [ ((963,626), 
     (997,663))]

nogoPrior :: Obj -> R
nogoPrior o = sum $ map f nogo
          where  f sqr | within sqr (posx o) (posy o) = -1e100
                       | otherwise = 0

prevprior :: Obj -> (Obj -> R)
prevprior (Obj vx vy vrot _ _ _) 
      (Obj nvx nvy nvrot _ _ _) 
        =   PDF.gaussD vx (sdv/2) nvx -- "heavy tailed proposals"
          + PDF.gaussD vy (sdv/2) nvy
          + PDF.gaussD vrot (sdvrot/2) nvy

fileroot = reverse . takeWhile (/='/') . reverse . takeWhile (/='.')

track :: StaticParams -> Image -> String -> Int -> Int -> Obj -> StateT Seed IO [Obj]
track sp bgIm vidfnm startfrm nframes obj0 = do
  let outFnm = fileroot vidfnm ++ ".pos"
  h<- lift $ openFile outFnm WriteMode 
  res <- go h bgIm (replicate nparticles obj0) [startfrm..startfrm+nframes] 
  lift $ hClose h
  return res
   where
    go _ bgIm objs [] = return []
    go h bgIm objs (i:is) = do
           lift $ system $"~/cvutils/extract "++vidfnm++" "++show i 
           frame <- lift $ readImage "extract.png"
           diffObjs <- sample $ mapM evolve objs
           lift $ print2  "pixel1Red = " (frame!(0,0,0))
           let wparticles = dropLosers $ particleLike sp bgIm frame diffObjs
           let anyInvisible = any objCentreInvisible $ map fst wparticles
           let npart = if anyInvisible then 5*nparticles else nparticles
           let wparticles' = if anyInvisible then map (\(x,w) -> (x,w/5)) wparticles else wparticles

           let smws = sumWeights wparticles'
           let cummSmws = cummWeightedSamples wparticles'
           lift $ print2 "npart=" npart          
           lift $ print2 "nwinners= " (length wparticles')
           lift $ hFlush stdout
           nextObjs <- sample $ sequence 
                              $ replicate npart
                              $ do u <- unitSample
                                   return . fst . fromJust $ find ((>=u*smws) . snd) cummSmws
           --lift $ print $ map snd $ take 10 $ reverse $ cummSmws

           let mobj 
                 = runStat (pure Obj <*> before meanF velx
                                     <*> before meanF vely
                                     <*> before meanF vrot
                                     <*> before meanF posx
                                     <*> before meanF posy
                                     <*> before meanF rot)
                              nextObjs
           lift $ putStrLn $ show (i,mobj, snd $ last $  wparticles)
           lift $ hPutStrLn h $ show (i,mobj, snd $ last $  wparticles)
           lift $ hFlush h
           when (i `rem` 5 == 0) $ do 
             markedIm1 <- lift $ markEllipse sp (mobj) frame     
             markedIm <- lift $ markObjsOnImage nextObjs markedIm1
             lift $ writeImage ("frame"++show i++".png") markedIm
           rest <- go h bgIm nextObjs is
           return $ mobj:rest
          
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
nparticles = 500

maxOn f xs = ceiling $ foldl1' max $ map f xs
minOn f xs = floor $ foldl1' min $ map f xs
 
data StaticParams = SP {noise :: !R,
                        wlength :: !R,
                        eccentric :: !R } deriving Show

particleLike :: StaticParams -> Image -> Image -> [(Obj,Obj)] -> [(Obj,R)]
particleLike sp@(SP noise len ecc) bgim im objprs = map pL objprs where
  radiusi = 2* ceiling (len*ecc) + 2
  objs = map snd objprs
  xs = [minOn (posx) objs - radiusi..maxOn (posx) objs+radiusi ] -- calc region of interest from all objs
  ys = [minOn (posy) objs-radiusi..maxOn posy objs+radiusi]
  pL (old,o@(Obj _ _ _ cx cy rot)) = 
    let f1x = cx+(len*ecc)*cos rot
        f1y = cy+(len*ecc)*sin rot
        f2x = cx-(len*ecc)*cos rot
        f2y = cy-(len*ecc)*sin rot
        f rot cx cy x y  
          | not $ visible x y = 0
          | dist  f1x f1y   x  y  + dist  f2x f2y   x  y  < 2 * len 
               =  {-# SCC "gaussrn" #-} (gaussRnn noise 0 $ im!(y,x,0)) + (gaussRnn noise 0 $ im!(y,x,1)) + (gaussRnn noise 0 $ im!(y,x,2))
          | otherwise = 
              {-# SCC "gaussw8rn" #-} (gaussW8nn noise (bgim!(y,x,0)) $ im!(y,x,0))+ (gaussW8nn noise (bgim!(y,x,1)) $ im!(y,x,1)) 
                   +(gaussW8nn noise (bgim!(y,x,2)) $ im!(y,x,2))
    in (o, nogoPrior o + prevprior old o + {-# SCC "fsum" #-} (sum [ f rot cx cy x y  | 
                   x <- xs, 
                   y <- ys]))

dist :: R -> R -> Int ->Int -> R 
dist  cx cy   x y  = sqrt((cx-(realToFrac x+0.5))^2+(cy-(realToFrac y+0.5))^2)

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
   where lpdf x = negate $ (((x-mu)^2)*tau)
         mu = word8ToR  muw8
gaussRnn :: R-> R -> Word8 -> R
gaussRnn tau mu = lpdf . word8ToR  
   where lpdf :: R -> R
         lpdf x =  negate $ (((x-mu)^2)*tau)


word8ToR :: Word8 -> Double
word8ToR = (/256) . realToFrac 
realToW8 :: Double -> Word8
realToW8 =  round. (*256) 

markObjsOnImage :: [Obj] -> Image -> IO (Image)
markObjsOnImage objs im = do
    mutIm <- thaw im
    forM_ objs $ \(Obj _ _ _ cx cy _) -> do
          writeArray mutIm (round cy, round cx, 2) 255
          writeArray mutIm (round cy, round cx, 0) 0
          writeArray mutIm (round cy, round cx, 1) 0

    freeze (mutIm::MImage)
    


         
mkRed arr cx cy = do 
          writeArray arr ( cy,
                             cx, 0) 255
--          writeArray arr ( cy,
--                             cx, 1) 0
--          writeArray arr ( cy,
--                             cx, 2) 0

main = do
     ilInit
     bgnm : fvid : (read -> x) : (read -> y) : (read -> rot) :_ <- getArgs
     --system $ "~/cvutils/extract "++fvid++" 1"
     bgIm <-readImage bgnm
     --frame0 <-readImage "extract.png"
     {-marked <- markObjsOnImage [Obj (x,y) 0, 
                                Obj (x+1,y) 0, 
                                Obj (x,y+1) 0, 
                                Obj (x+1,y+1) 0] frame0
     writeImage "marked.png" marked -}
     let --posterior = posteriorV bgIm frame0 (round x,round y)
         --postAndV v = (posteriorV bgIm frame0 (round x,round y) $ fromList v, v)
         sp = (SP 218 6 0.9)
--         rot = negate (pi/7)
         --initialsV = fromList [x,y,218, 6, rot, 0.9]
         initObj =  (Obj 0 0 0 x y rot) 
     --ellim <- markEllipse sp initObj frame0
     --writeImage "markell.png" ellim
     print $ nogoPrior (Obj 0 0 0 992 650 rot)
     print $ nogoPrior (Obj 0 0 0 997 650 rot)
     print $ visible 994 650
     print $ visible 1000 650

     runRIO $ do
--         iniampar <- sample $ initialAdaMet 500 1e-4 posterior initialsV
--         AMPar v _ _ _ <- runAndDiscard 5000 (show . ampPar) iniampar $ adaMet False posterior
--         lift $ print v
--         track (v@> 2) (v @> 3) bgIm fvid 3 (v@>0,v@>1)
         track sp bgIm fvid 1020 50 $ initObj
     
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
markEllipse sp@(SP noise len ecc) (Obj _ _ _ px py rot) im = do
    mutIm <- thaw im
    
    let cx = round px
    let cy = round py
        f1x = px+(len*ecc)*cos rot
        f1y = py+(len*ecc)*sin rot
        f2x = px-(len*ecc)*cos rot
        f2y = py-(len*ecc)*sin rot
    forM [627, 630..663] $ \y-> do
       writeArray mutIm (y, 996, 2) 255
       writeArray mutIm (y, 999, 2) 255
       writeArray mutIm (y, 963, 2) 255
--    print sp
--    print (f1x, f1y)
--    print (f2x, f2y)
--    print $ dist (f1x,f1y) (cx, cy)
    let f x y = do -- print2 "f at" (x,y,dist (f1x,f1y) (x, y) + dist (f2x,f2y) (x, y) )
                   when (dist  f1x f1y   x  y  + dist  f2x f2y   x  y  < 2 * len) $ do
                      --print2 "red at " (x,y) 
                      --mkRed mutIm x 
                      return ()
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
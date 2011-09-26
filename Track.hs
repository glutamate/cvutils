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
 
import Data.IORef
import System.IO.Unsafe

import CVUtils
import Edge

bgImRef :: IORef Image
bgImRef = unsafePerformIO $ newIORef undefined



sdv = 0.5
sdrot = 0.2
sddiv = 2
sdSideDisp = 0.3

hiddenflat = 1

nparticles = 1000

evolve :: Obj -> Sampler (Obj,Obj)
evolve o@(Obj vlen side x y rot) = do
        nvlen <- gaussD vlen sdv
        nside <- gaussD 0 sdSideDisp
        nrot <- gaussD rot sdrot
        let no = Obj nvlen nside (x+nvlen*cos nrot+nside*cos(nrot-pi/2)) 
                                 (y+nvlen*sin nrot+nside*sin(nrot-pi/2)) nrot
        if nogoObj no 
           then evolve o 
           else return $ (o,no)

invisible :: [((Int,Int),(Int,Int))]

invisible = [((362,621),(355,654)),
             ((442,649),(443,674)),
             ((866,626),(870,658)),
             ((995,628),(999,657)),
             ((1149,629),(1167,658)),
             ((1003,71),(1010,36)),
             ((422,71),(417,42)),
             ((157,162),(127,158))]

--[ ((963,626), 
--               (999,663))]

visible :: Int -> Int -> Bool
visible x y  = not $ any (\sqr -> within sqr  x  y) invisible

objCentreInvisible :: Obj -> Bool
objCentreInvisible (Obj _ _ cx cy _) = not $ visible (round cx) (round cy)


nogo::  [Square]
nogo = 
    [((1128,657.5),(1162,627.5)),
     ((963.25,656),(997,626.5)),
     ((836.5,653.75),(866.75,624.5)),
     ((389.75,619.5),(359.75,648.75)),
     ((162.75,309),(132,278.5)),
     ((158.5,191.25),(127.25,160.75)),
     ((422,72.25),(450.75,40.5)),
     ((757.5,72),(728.5,41.75)),
     ((1002.75,70.5),(974.25,39.75)),
     ((1191,194),(1221,222.75)),
     ((1185,435.25),(1213.5,463.5))]
--  [ ((963,626), 
--     (997,663))]

nogoPrior :: Obj -> R
nogoPrior o = sum $ map f nogo
          where  f sqr | within sqr (posx o) (posy o) = -1e100
                       | otherwise = 0

nogoObj :: Obj -> Bool
nogoObj o = any (\sqr-> within sqr (posx o) (posy o)) nogo


prevprior :: Obj -> (Obj -> R)
prevprior (Obj vlen side x y rot) 
      (Obj nvlen nside _ _ nrot) 
        =   PDF.gaussD vlen (sdv/sddiv) nvlen -- "heavy tailed proposals"
          + PDF.gaussD rot (sdrot/sddiv) nrot
          + PDF.gaussD 0 (sdSideDisp/sddiv) nside

track :: StaticParams -> String -> Int -> Int -> Obj -> StateT Seed IO [Obj]
track sp vidfnm startfrm nframes obj0 = do
  let outFnm = fileroot vidfnm ++ ".pos"
  h<- lift $ openFile outFnm WriteMode 
  res <- go h (replicate nparticles obj0) [startfrm..startfrm+nframes-1] 
  lift $ hClose h
  return res
   where
    go _ objs [] = return []
    go h objs (i:is) = do
           bgIm <- lift $ readIORef bgImRef
           lift $ putStrLn $"~/cvutils/extract "++vidfnm++" "++show i 
           lift $ system $"~/cvutils/extract "++vidfnm++" "++show i 
           frame <- lift $ readImage "extract.png"
           diffObjs <- sample $ mapM evolve objs
           --lift $ print2  "pixel1Red = " (frame!(0,0,0))
           let wparticles = {-# SCC "wpart" #-} (dropLosers $ particleLike sp bgIm frame diffObjs)
           let anyInvisible = any objCentreInvisible $ map fst wparticles
           let npart = if anyInvisible then 20*nparticles else nparticles
           let wparticles' = if anyInvisible then map (\(x,w) -> (x,w/hiddenflat)) wparticles else wparticles

           let smws = sumWeights wparticles'
           let cummSmws = cummWeightedSamples wparticles'
           lift $ print2 "npart=" npart          
           lift $ print2 "winners= " (map snd wparticles')
           lift $ hFlush stdout
           nextObjs <- sample $ sequence 
                              $ replicate npart
                              $ do u <- unitSample
                                   return . fst . fromJust $ find ((>=u*smws) . snd) cummSmws
           --lift $ print $ map snd $ take 10 $ reverse $ cummSmws

           let mobj 
                 = runStat (pure Obj <*> before meanF vellen
                                     <*> before meanF sideDisp
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
             lift $ updateBgIm frame mobj
             --bgIm2 <- lift $ readIORef bgImRef
             --lift $ writeImage ("bgtrack"++show i++".png") bgIm2
           rest <- go h nextObjs is
           return $ mobj:rest

particleLike :: StaticParams -> Image -> Image -> [(Obj,Obj)] -> [(Obj,R)]
particleLike sp@(SP noise len ecc) bgim im objprs = {-# SCC "particleLike" #-} (map pL objprs) where
  radiusi = 2* ceiling (len*ecc) + 2
  objs = map snd objprs
  xmin =minOn (posx) objs - radiusi 
  xmax = maxOn (posx) objs+radiusi
  ymin = minOn (posy) objs-radiusi
  ymax = maxOn posy objs+radiusi
  xs = [xmin.. xmax] -- calc region of interest from all objs
  ys = [ymin..ymax]
  ifInside, ifoutside, ifmixed ::UArray (Int,Int) R
  ifInside = array ((xmin,ymin),(xmax,ymax)) 
                   [((x,y),(gaussRnn noise 0 $ im!(y,x,0)) 
                           + (gaussRnn noise 0 $ im!(y,x,1)) 
                           + (gaussRnn noise 0 $ im!(y,x,2)))
                       | x <- xs, 
                         y <- ys]
  ifoutside = array ((xmin,ymin),(xmax,ymax)) 
                    [ ((x,y),(gaussW8nn noise (bgim!(y,x,0)) $ im!(y,x,0))
                             + (gaussW8nn noise (bgim!(y,x,1)) $ im!(y,x,1)) 
                             + (gaussW8nn noise (bgim!(y,x,2)) $ im!(y,x,2)))
                       | x <- xs, 
                         y <- ys]
  ifmixed   = array ((xmin,ymin),(xmax,ymax)) 
                    [ ((x,y),(gaussW8nn noise (bgim!(y,x,0) `div` 2) $ im!(y,x,0))
                             + (gaussW8nn noise (bgim!(y,x,1) `div` 2) $ im!(y,x,1)) 
                             + (gaussW8nn noise (bgim!(y,x,2) `div` 2) $ im!(y,x,2)))
                       | x <- xs, 
                         y <- ys]
 
  pL (old,o@(Obj  _ _ cx cy rot)) = 
    let f1x = cx+(len*ecc)*cos rot
        f1y = cy+(len*ecc)*sin rot
        f2x = cx-(len*ecc)*cos rot
        f2y = cy-(len*ecc)*sin rot
        sqrlen = 4 * len* len
        f x y  
          | not $ visible x y = 0
          | dist  f1x f1y   x  y  + dist  f2x f2y   x  y  < 2 * len 
               =  ifInside!(x,y)
          | otherwise =  ifoutside!(x,y)
    in if nogoObj o then (o,-1e100) else (o, nogoPrior o + prevprior old o + {-# SCC "fsum" #-} (noise * sum [ f x y  | 
                   x <- xs, 
                   y <- ys]))

updateBgIm frame (Obj _ _ cx cy _) = do
    im <- readIORef bgImRef
    mutIm <- thaw im
    ((loy,lox,_),(hiy,hix,_)) <- getBounds mutIm
    forM_ [(y,x,c) | x<- [lox..hix], 
                     y<- [loy..hiy],
                     c<- [0..2], 
                     dist cx cy x y > 30] $ \ix-> do
      now <- readArray mutIm ix
      writeArray mutIm ix $ (now `div` 2)  + ((frame!ix) `div` 2)
    newbg <- freeze (mutIm::MImage)
    writeIORef bgImRef newbg

main = do
     ilInit
     bgnm : fvid : (read -> x) : (read -> y) : (read -> rot) :_ <- getArgs
     --system $ "~/cvutils/extract "++fvid++" 1"
     bgIm <-readImage bgnm
     writeIORef bgImRef bgIm
     marked <- markBg bgIm
     writeImage "markbg.png" marked
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
         initObj =  (Obj 1 rot x y rot) 
     --ellim <- markEllipse sp initObj frame0
     --writeImage "markell.png" ellim
     print $ nogoPrior (Obj 0 rot 992 650 rot)
     print $ nogoPrior (Obj 0 rot 997 650 rot)
     print $ visible 994 650
     print $ visible 1000 650

     runRIO $ do
--         iniampar <- sample $ initialAdaMet 500 1e-4 posterior initialsV
--         AMPar v _ _ _ <- runAndDiscard 5000 (show . ampPar) iniampar $ adaMet False posterior
--         lift $ print v
--         track (v@> 2) (v @> 3) bgIm fvid 3 (v@>0,v@>1)
         track sp fvid 3200 1000 $ initObj
     
     return () 

-- initial on wl0: (956,641)

--rm marked.png && track mixed.png ~/Dropbox/woodlice/wl0.avi '(956,641)' && eog marked.png
--rm -f frame*.png && sudo cabal install --global && track mixed.png ~/Dropbox/woodlice/wl0.avi 956 641 '-0.448'


--rm -f frame*.png && sudo cabal install --global && track mixed.png ~/Dropbox/woodlice/wl0.avi 1092 518 '-5.12'
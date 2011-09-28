{-# LANGUAGE ViewPatterns #-}

module Edge where

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
import Data.List hiding (map)
import Data.Maybe
import Control.Applicative
import Data.Ord
import qualified Data.Vector as V
import qualified Graphics.Rendering.OpenGL.GLU.Tessellation as GLU
import Graphics.Rendering.OpenGL

import CVUtils
import qualified Graphics.UI.GLUT as GLUT

loadPoints :: IO [(Double,Double)]
loadPoints = fmap read $ readFile "edge.dat"


complexPolygon :: [(Double,Double)] -> GLU.ComplexPolygon GLfloat
complexPolygon points
  = let p2v (x,y) = Vertex3 (realToFrac x) (realToFrac y) 0
    in GLU.ComplexPolygon 
         [GLU.ComplexContour $map (\v-> GLU.AnnotatedVertex (p2v v) 0) points]

noOpCombiner _newVertex _weightedProperties = 0.0 ::GLfloat

triangulateEdge :: [(Double,Double)] -> IO [[Vector Double]]
triangulateEdge pts = do
   fmap getTriangles $ GLU.triangulate
            GLU.TessWindingPositive 0 (Normal3 0 0 0) noOpCombiner
            $ complexPolygon pts

type Triangles = [[Vector Double]]
     
getTriangles (GLU.Triangulation tris) = map unTri tris where
   unTri 
    (GLU.Triangle (GLU.AnnotatedVertex (Vertex3 x1 y1 _) _) 
                  (GLU.AnnotatedVertex (Vertex3 x2 y2 _) _) 
                  (GLU.AnnotatedVertex (Vertex3 x3 y3 _) _)) 
       = [fromList [realToFrac x1,realToFrac y1], 
          fromList [realToFrac x2,realToFrac y2], 
          fromList [realToFrac x3,realToFrac y3]]

--http://frame3dd.svn.sourceforge.net/viewvc/frame3dd/trunk/src/microstran/vec3.c?view=markup
cross a b = fromList [(a@>1)*(b@>2)-(a@>2)*(b@>1)
                     ,(a@>2)*(b@>0)-(a@>0)*(b@>2)
                     ,(a@>0)*(b@>1)-(a@>1)*(b@>0)]

crossZ x y = (x@>0)*(y@>1)-(x@>1)*(y@>0)

crossZp a b p = ((b@>0)-(a@>0))*((p@>1)-(a@>1))-((b@>1)-(a@>1))*((p@>0)-(a@>0))

--http://www.blackpawn.com/texts/pointinpoly/default.html

sameSide :: (Vector Double,Vector Double,Vector Double,Vector Double) -> Bool
sameSide(p1,p2, a,b) 
 = let cp1 = crossZ (b-a) (p1-a)
       cp2 = crossZ (b-a) (p2-a)
    in cp1 * cp2 >= 0 

sameSide' :: (Vector Double,Vector Double,Vector Double,Vector Double) -> Bool
sameSide'(p1,p2, a,b) 
 = let cp1 = crossZp a b p1
       cp2 = crossZp a b p2
    in cp1 * cp2 >= 0 



pointInTriangle :: Vector Double -> [Vector Double] -> Bool
pointInTriangle p [a,b,c]
    = sameSide'(p,a, b,c) && sameSide'(p,b, a,c)
        && sameSide'(p,c, a,b) 


markBg :: Image -> IO Image
markBg im = do
    (progName,args) <-  GLUT.getArgsAndInitialize
    pts <- loadPoints
    tris <- triangulateEdge pts
 --   print tris
    print $ head tris
    --print $ cross (head tris!!0) (head tris!!1)

--    print $ pointInTriangle (fromList [100,100]) $ head tris
    mutIm <- thaw im
    ((loy,lox,_),(hiy,hix,_)) <- getBounds (mutIm::MImage)
    forM_ [(y,x,0) | x<- [lox..hix], 
                     y<- [loy..hiy]] $ \ix@(y,x,_)-> do
      when (any (pointInTriangle (fromList [realToFrac x, realToFrac y])) tris) $ writeArray mutIm ix 255
    freeze (mutIm::MImage) 
--    return im

loadTriangles :: IO [[Vector Double]] 
loadTriangles = do
    (progName,args) <-  GLUT.getArgsAndInitialize
    pts <- loadPoints
    triangulateEdge pts

writeVisMat :: String -> BitImage -> Image -> IO ()
writeVisMat nm vm im = do
   mutIm <- thaw im
   ((loy,lox,_),(hiy,hix,_)) <- getBounds (mutIm::MImage)
   forM_ [(y,x,0) | x<- [lox..hix], 
                     y<- [loy..hiy]] $ \ix@(y,x,_)-> do
     when (not $ readBitImage vm x y) $ writeArray mutIm ix 255
   

   newIm <- freeze (mutIm::MImage)
   writeImage nm newIm
   return ()
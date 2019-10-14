module Main where

import Data.Char
import Lib
import System.Environment (getArgs)
import qualified Vec
import qualified Shape
import Function (radiance, intersectionGen)
import System.Random
import Debug.Trace 
import Control.Parallel.Strategies
import Control.Parallel

interactWith function params outputFile = do
    writeFile outputFile (function params)

main :: IO ()
main = mainWith myFunction
    where
        mainWith function = do 
            args <- getArgs
            rGen <- newStdGen
            case args of 
                [input, output] -> interactWith function (read input) output
                _ -> putStrLn "error: exactly two arguments needed" 
        myFunction = generatePPM.parRender

parRender sams = map renderRow [767, 766..0]
    where 
        renderRow y = parRenderRow sams (showValue "rendering row" y)

parRenderRow sams i = parMap rpar sample [0..1023]
    where sample x = (sampleFunc sams) x i

sampleFunc sams = \x y -> sampleAt (mkStdGen (y*1024+x+2048)) cam scene (fromIntegral x) (fromIntegral y) sams

cam = Shape.Ray (Vec.Vec 50 52 295.6) (Vec.normVec (Vec.Vec 0 (-0.042612) (-1)))
scene = intersectionGen eleList
eleList = [ 
    (Shape.Sphere 1e5  (Vec.Vec (1e5+1) 40.8 81.6)       (Vec.Vec 0 0 0)     (Vec.Vec 0.75 0.25 0.25)       Shape.Diff), 
    (Shape.Sphere 1e5  (Vec.Vec ((-1e5)+99) 40.8 81.6)   (Vec.Vec 0 0 0)     (Vec.Vec 0.25 0.25 0.75)       Shape.Diff),
    (Shape.Sphere 1e5  (Vec.Vec 50 40.8 1e5)             (Vec.Vec 0 0 0)     (Vec.Vec 0.25 0.75 0.25)       Shape.Diff),
    (Shape.Sphere 1e5  (Vec.Vec 50 40.8 ((-1e5)+170))    (Vec.Vec 0 0 0)     (Vec.Vec 0 0 0)                Shape.Diff),
    (Shape.Sphere 1e5  (Vec.Vec 50 1e5 81.6)             (Vec.Vec 0 0 0)     (Vec.Vec 0.75 0.75 0.75)       Shape.Diff),
    (Shape.Sphere 1e5  (Vec.Vec 50 ((-1e5)+81.6) 81.6)   (Vec.Vec 0 0 0)     (Vec.Vec 0.75 0.75 0.75)       Shape.Diff),
    (Shape.Sphere 16.5 (Vec.Vec 27 16.5 47)              (Vec.Vec 0 0 0)     (Vec.Vec 0.8 0.8 0.8)          Shape.Spec),
    (Shape.Sphere 16.5 (Vec.Vec 73 16.5 78)              (Vec.Vec 0 0 0)     (Vec.Vec 0.999 0.999 0.999)    Shape.Refr),
    (Shape.Sphere 600  (Vec.Vec 50 (681.6-0.27) 81.6)    (Vec.Vec 12 12 12)  (Vec.Vec 0 0 0)                Shape.Diff)]

sampleAt :: (RandomGen g) => g -> Shape.Ray -> (Shape.Ray -> (Maybe Shape.Element, Maybe Double)) -> Double -> Double -> Int -> Vec.Vec
sampleAt randGen _ _ _ _  0 = Vec.zero
sampleAt randGen cam scene x y sams = (Vec.scaleVec (foldr (+) Vec.zero vList) 0.25)
    where
        rayList = generateRay cam x y
        vList = getValue rayList randGen
        getValue [] rG = []
        getValue (r:rs) rG = (clamp v):r1
            where
                v = sampleNTimes scene r rG0 sams
                r1 = getValue rs rG1
                (rG0, rG1) = split rG

sampleNTimes :: (RandomGen g) => (Shape.Ray -> (Maybe Shape.Element, Maybe Double)) -> Shape.Ray -> g -> Int -> Vec.Vec
sampleNTimes scene ray rndGen samples = sampleNTimes' scene ray rndGen scaleFactor samples
    where
        scaleFactor = 1/(fromIntegral samples)
        sampleNTimes' s r rG sF 0 = Vec.zero
        sampleNTimes' s r rG sF step = (Vec.scaleVec v sF) + nextR
            where
                v = (radiance scene 0 r rG0)
                nextR = sampleNTimes' scene r rG1 sF (step-1)
                (rG0, rG1) = split rG

clamp :: Vec.Vec -> Vec.Vec 
clamp v = Vec.Vec x y z
    where
        x = if Vec.x v < 0 then 0 else (if Vec.x v > 1 then 1 else Vec.x v)
        y = if Vec.y v < 0 then 0 else (if Vec.y v > 1 then 1 else Vec.y v)
        z = if Vec.z v < 0 then 0 else (if Vec.z v > 1 then 1 else Vec.z v)


generateRay :: Shape.Ray -> Double -> Double -> [Shape.Ray]
generateRay (Shape.Ray camO camD) x y = [Shape.Ray (camO + (Vec.scaleVec (Vec.normVec d) 140)) (Vec.normVec (d)) | d <- dList]
    where
        dList = [((Vec.scaleVec cx scalex) + (Vec.scaleVec cy scaley)) + camD | (scalex, scaley) <- scalexy]
        cx = (Vec.Vec (1024*0.5135/768) 0 0)
        cy = (Vec.scaleVec (Vec.normVec (Vec.crossVec cx camD)) 0.5135)
        scalexy = [(((((sx + 0.5) / 2 + x)/1024 - 0.5)), ((((sy + 0.5) / 2 + y)/768 - 0.5))) | sx <- [0,1], sy <- [0,1]]

showValue name x = trace ("Value "++name++" :" ++ show(x)) x

generatePPM :: [[Vec.Vec]] -> String
generatePPM image = outStr
    where
        head = "P3\n1024 768\n255\n"
        body = getImageBody image
        outStr = head ++ body 

getImageRow :: [Vec.Vec] -> String
getImageRow [] = "\n"
getImageRow (x:xs) = (show v_x) ++ " " ++ (show v_y) ++ " "++ (show v_z) ++ " " ++ (getImageRow xs)
    where
        v_x = toInt (Vec.x x)
        v_y = toInt (Vec.y x)
        v_z = toInt (Vec.z x)

getImageBody :: [[Vec.Vec]] -> String
getImageBody [] = []
getImageBody (x:xs) = (getImageRow x) ++ (getImageBody xs)

toInt :: Double -> Int 
toInt v 
    | v <= 0 = 0
    | v >= 1 = 255
    | otherwise = floor ((v ** (1/2.2)) * 255 + 0.5)


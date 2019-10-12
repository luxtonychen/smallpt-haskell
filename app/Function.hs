module Function where

import qualified Vec
import qualified Shape
import Data.Maybe
import System.Random
import Debug.Trace

type NextRay = (Shape.Ray, Maybe Shape.Ray, Maybe Double, Maybe Double)

radiance :: (RandomGen g) => (Integer, [Shape.Element], Shape.Ray, g) -> (Vec.Vec, g)
radiance input = (getRetValue.getNextRet.getNextRay.getSelfEmCo.intersection) input

intersection :: (RandomGen g) => (Integer, [Shape.Element], Shape.Ray, g) -> (Integer, [Shape.Element], Shape.Ray, Maybe Shape.Element, Maybe Double, g)
intersection (depth, scene, ray, randGen) = (depth, scene, ray, ele, t, nextRandGen)
    where
        (t, ele) = intersection' scene ray Nothing Nothing
        nextRandGen = randGen

getSelfEmCo :: (RandomGen g) => (Integer, [Shape.Element], Shape.Ray, Maybe Shape.Element, Maybe Double, g) -> (Maybe (Integer, [Shape.Element], Shape.Ray, Shape.Element, Double, Vec.Vec), Vec.Vec, g)
getSelfEmCo (_, _, _, _, Nothing, randGen) = (Nothing, Vec.Vec 0 0 0, randGen)
getSelfEmCo (depth, scene, ray, Just ele, Just t, randGen)
    | cond = (Nothing, emission, nextRandGen)
    | otherwise = (Just (depth, scene, ray, ele, t, color), emission, nextRandGen)
    where 
        (cond, scaleCond, nextRandGen) = depthCond randGen depth (Vec.maxDim f)
        f = Shape.color ele
        emission = Shape.emission ele
        color = if scaleCond then Vec.scaleVec f (1/(Vec.maxDim f)) else f

getNextRay :: (RandomGen g) => (Maybe (Integer, [Shape.Element], Shape.Ray, Shape.Element, Double, Vec.Vec), Vec.Vec, g) -> (Maybe (Integer, [Shape.Element], NextRay, Vec.Vec), Vec.Vec, g)
getNextRay (Nothing, e, randGen) = (Nothing, e, randGen)
getNextRay (Just (depth, scene, ray, ele, t, color), emission, randGen) = (Just (depth, scene, nextRay, color), emission, nextRandGen)
    where (nextRay, nextRandGen) = getNextRay' (ele, t, ray) randGen


getNextRet :: (RandomGen g) => (Maybe (Integer, [Shape.Element], NextRay, Vec.Vec), Vec.Vec, g) -> (Maybe (Vec.Vec, Vec.Vec), Vec.Vec, g)
getNextRet (Nothing, e, randGen) = (Nothing, e, randGen)
getNextRet (Just (depth, scene, (nextRay, Nothing, _, _), color), emission, randGen) = (Just (retVec, color), emission, nextRandGen)
    where (retVec, nextRandGen) = radiance ((depth+1), scene, nextRay, randGen)
getNextRet (Just (depth, scene, (nextRayRefl, Just nextRayRefr, Just re, Just tr), color), emission, randGen) = (Just (retVec, color), emission, nextRandGen)
    where 
        (s1, s2, nextGen) = refrCond randGen depth p 
        state = (s1, s2)
        p = 0.25 + 0.5 * re
        rp = (re / p)
        tp = (tr / (1-p))
        (randGenRefl, randGenRefr) = split nextGen
        (retRefl, nextRandGen1) = radiance ((depth+1), scene, nextRayRefl, randGenRefl)
        (retRefr, nextRandGen2) = radiance ((depth+1), scene, nextRayRefr, randGenRefr)
        (retVec, nextRandGen) =  case state of
            (True, True)    ->  ((Vec.scaleVec retRefl rp), nextRandGen1)
            (True, False)   ->  ((Vec.scaleVec retRefr tp), nextRandGen2)
            _               ->  (Vec.addVec (Vec.scaleVec retRefl re) (Vec.scaleVec retRefr tr), nextRandGen2)

    

getRetValue :: (RandomGen g) => (Maybe (Vec.Vec, Vec.Vec), Vec.Vec, g) -> (Vec.Vec, g)
getRetValue (Nothing, e, randGen) = (e, randGen)
getRetValue (Just (retVec, color), emission, randGen) = ((Vec.addVec emission (Vec.multVec color retVec)), randGen) 


multAddRadiance :: (RandomGen g) => (Vec.Vec, g) -> (Vec.Vec, g) -> Double -> Double -> (Vec.Vec, g)
multAddRadiance (rt1, gen1) (rt2, gen2) re tr = (Vec.addVec (Vec.scaleVec rt1 re) (Vec.scaleVec rt2 tr), gen2)                
                     

depthCond :: (RandomGen g) => g -> Integer -> Double -> (Bool, Bool, g)
depthCond randGen depth p
    | randNum >= p && depth > 5 = (True, scaleCond, nextRandGen)
    | otherwise = (False, scaleCond, nextRandGen)
    where
        (randNum, nextRandGen) = random randGen 
        scaleCond = randNum < p && depth > 5

refrCond :: (RandomGen g) => g -> Integer -> Double -> (Bool, Bool, g)
refrCond randGen depth p 
    | depth > 2 && randNum < p = (True, True, nextGen) 
    | depth > 2 && randNum >= p = (True, False, nextGen)
    | otherwise = (False, False, nextGen)
    where
        (randNum, nextGen) = random randGen

getP :: Maybe Shape.Element -> Double
getP Nothing = 0
getP (Just (Shape.Sphere _ _ _ f _)) = if (Vec.x f > Vec.y f) && (Vec.x f > Vec.z f) then Vec.x f else (if Vec.y f > Vec.z f then Vec.y f else Vec.z f)

randomN :: (RandomGen g, Random a) => g -> Int -> ([a], g)
randomN randGen 0 = ([], randGen)
randomN randGen n = (randNum:nextNum, nextGen)
        where
            (randNum, g) = random randGen
            (nextNum, nextGen) = randomN g (n-1)

getNextRay' :: (RandomGen g) => (Shape.Element, Double, Shape.Ray) -> g -> (NextRay, g) 
getNextRay' ((Shape.Sphere _ pos _ _ surface), t, (Shape.Ray rO rD)) randGen = 
    let 
        x = Vec.addVec rO (Vec.scaleVec rD t)
        n = Vec.normVec (Vec.subVec x pos)
        nl = if (Vec.dotVec n rD) < 0 then n else Vec.scaleVec n (-1)
        in case surface of
            Shape.Spec -> ((Shape.Ray x (Vec.subVec rD (Vec.scaleVec n (2 * (Vec.dotVec n rD)))), Nothing, Nothing, Nothing), randGen)
            Shape.Diff -> ((Shape.Ray x (Vec.normVec (Vec.addVec (Vec.addVec u' v') w')), Nothing, Nothing, Nothing), randGenRet )
                where
                    u' = Vec.scaleVec u ((cos r1) * r2s)
                    v' = Vec.scaleVec v ((sin r1) * r2s)
                    w' = Vec.scaleVec w (sqrt (1 - r2))
                    w = nl 
                    u = Vec.normVec (Vec.crossVec (if (abs (Vec.x w )) > 0.1 then (Vec.Vec 0 1 0) else (Vec.Vec 1 0 0)) w)
                    v = Vec.crossVec w u 
                    ( rnd1:rnd2:_, randGenRet) = randomN randGen 2
                    r1 = rnd1*2*pi 
                    r2 = rnd2 
                    r2s = sqrt rnd2
            Shape.Refr -> if cos2t < 0 then ((refl, Nothing, Nothing, Nothing), randGen ) else ((refl, Just refr, Just re, Just tr), randGen )
                where
                    refl = Shape.Ray x (Vec.subVec rD (Vec.scaleVec n (2 * (Vec.dotVec n rD))))
                    refr = Shape.Ray x tdir
                    re = r0 + (1 - r0)*(c**5)
                    tr = 1-re
                    into = Vec.dotVec n nl > 0
                    nc = 1
                    nt = 1.5
                    nnt = if into then nc/nt else nt/nc 
                    ddn = Vec.dotVec rD nl
                    cos2t = 1-nnt*nnt*(1-ddn*ddn)
                    tdir = if into 
                        then Vec.normVec (Vec.subVec (Vec.scaleVec rD nnt) (Vec.scaleVec n (ddn*nnt+(sqrt cos2t))))
                        else Vec.normVec (Vec.subVec (Vec.scaleVec rD nnt) (Vec.scaleVec n ((-1)*(ddn*nnt+(sqrt cos2t)))))
                    r0 = ((nt-nc)**2)/((nt+nc)**2)
                    c = 1 - (if into then (-1)*ddn else (Vec.dotVec tdir n))


--intersection :: [Shape.Element] -> Shape.Ray -> (Maybe Double, Maybe Shape.Element) 
--intersection eleList r = (t, e)
--    where
--        (t, e) = intersection' eleList r Nothing Nothing

intersection' :: [Shape.Element] -> Shape.Ray -> Maybe Double -> Maybe Shape.Element -> (Maybe Double, Maybe Shape.Element)
intersection' [] r t_min ele_min = (t_min, ele_min)
intersection' (x:xs) r t_min ele_min
    | (t_min == Nothing) && (t /= Nothing)  = intersection' xs r t (Just x)
    | t == Nothing                          = intersection' xs r t_min ele_min
    | t < t_min                             = intersection' xs r t (Just x)
    | otherwise                             = intersection' xs r t_min ele_min
    where
        t = intersect x r

intersect :: Shape.Element -> Shape.Ray -> Maybe Double
intersect (Shape.Sphere r p _ _ _) (Shape.Ray o d)
    |det < 0    = Nothing
    |t1 > eps   = Just t1
    |t2 > eps   = Just t2
    |otherwise  = Nothing
    where
        v_op = Vec.subVec p o
        eps = 0.0001
        b = Vec.dotVec v_op d
        det = (b**2) - (Vec.dotVec v_op v_op) + (r**2)
        t1 = b - det**(1/2)
        t2 = b + det**(1/2)

intersect _ _ = Nothing
module Function where

import qualified Vec
import qualified Shape
import Data.Maybe
import System.Random
import Debug.Trace

type NextRay = (Shape.Ray, Maybe Shape.Ray, Maybe Double, Maybe Double)

radiance :: (RandomGen g) => (Shape.Ray -> (Maybe Shape.Element, Maybe Double)) -> Integer -> Shape.Ray -> g -> Vec.Vec
radiance interFunc depth ray randGen
    | color == Nothing  = emission
    | otherwise         = emission + ((fromJust color) * retNext)
    where 
        (emission, color) = getSelfEmCo depth ele t randGen1
        retNext = getNextRet interFunc depth (getNextRay ele t ray randGen2) randGen3
        (ele, t) = interFunc ray
        (randGen0, randGen1) = split randGen
        (randGen2, randGen3) = split randGen0

intersectionGen :: [Shape.Element] -> (Shape.Ray -> (Maybe Shape.Element, Maybe Double))
intersectionGen []      = \r -> (Nothing, Nothing)
intersectionGen eleList = \r -> intersection' eleList r Nothing Nothing

intersection' :: [Shape.Element] -> Shape.Ray -> Maybe Double -> Maybe Shape.Element -> (Maybe Shape.Element, Maybe Double)
intersection' [] r t_min ele_min = (ele_min, t_min)
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
        v_op = p - o
        eps = 0.0001
        b = Vec.dotVec v_op d
        det = (b**2) - (Vec.dotVec v_op v_op) + (r**2)
        t1 = b - det**(1/2)
        t2 = b + det**(1/2)

intersect _ _ = Nothing

getSelfEmCo :: (RandomGen g) => Integer -> Maybe Shape.Element -> Maybe Double -> g -> (Vec.Vec, Maybe Vec.Vec)
getSelfEmCo _ Nothing _ randGen = (Vec.zero, Nothing)
getSelfEmCo depth (Just ele) (Just t) randGen
    | cond = (emission, Nothing)
    | otherwise = (emission, Just color)
    where 
        (cond, scaleCond) = depthCond randGen depth (Vec.maxDim f)
        f = Shape.color ele
        emission = Shape.emission ele
        color = if scaleCond then Vec.scaleVec f (1/(Vec.maxDim f)) else f

getNextRay :: (RandomGen g) => Maybe Shape.Element -> Maybe Double -> Shape.Ray -> g -> Maybe NextRay
getNextRay Nothing _ _ _ = Nothing
getNextRay (Just ele) (Just t) ray randGen = Just (getNextRay' (ele, t, ray) randGen)


getNextRet :: (RandomGen g) => (Shape.Ray -> (Maybe Shape.Element, Maybe Double)) -> Integer -> Maybe NextRay -> g -> Vec.Vec
getNextRet interFunc depth Nothing randGen = Vec.zero
getNextRet interFunc depth (Just (nextRayRefl ,Nothing ,_ ,_)) randGen = radiance interFunc (depth+1) nextRayRefl randGen
getNextRet interFunc depth (Just (nextRayRefl, Just nextRayRefr, Just re, Just tr)) randGen = retVec
    where 
        (s1, s2, nextGen) = refrCond randGen depth p 
        state = (s1, s2)
        p = 0.25 + 0.5 * re
        rp = (re / p)
        tp = (tr / (1-p))
        (randGenRefl, randGenRefr) = split nextGen
        retRefl = radiance interFunc (depth+1) nextRayRefl randGenRefl
        retRefr = radiance interFunc (depth+1) nextRayRefr randGenRefr
        retVec =  case state of
            (True, True)    ->  (Vec.scaleVec retRefl rp)
            (True, False)   ->  (Vec.scaleVec retRefr tp)
            _               ->  (Vec.scaleVec retRefl re) + (Vec.scaleVec retRefr tr)
                     

depthCond :: (RandomGen g) => g -> Integer -> Double -> (Bool, Bool)
depthCond randGen depth p
    | randNum >= p && depth > 5 = (True, scaleCond)
    | otherwise = (False, scaleCond)
    where
        (randNum, _) = random randGen 
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

getNextRay' :: (RandomGen g) => (Shape.Element, Double, Shape.Ray) -> g -> NextRay
getNextRay' ((Shape.Sphere _ pos _ _ surface), t, (Shape.Ray rO rD)) randGen = 
    let 
        x = rO + (Vec.scaleVec rD t)
        n = Vec.normVec (x - pos)
        nl = if (Vec.dotVec n rD) < 0 then n else Vec.scaleVec n (-1)
        in case surface of
            Shape.Spec -> (Shape.Ray x (rD - (Vec.scaleVec n (2 * (Vec.dotVec n rD)))), Nothing, Nothing, Nothing)
            Shape.Diff -> (Shape.Ray x (Vec.normVec ((u' + v') + w')), Nothing, Nothing, Nothing)
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
            Shape.Refr -> if cos2t < 0 then (refl, Nothing, Nothing, Nothing) else (refl, Just refr, Just re, Just tr)
                where
                    refl = Shape.Ray x (rD - (Vec.scaleVec n (2 * (Vec.dotVec n rD))))
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
                        then Vec.normVec ((Vec.scaleVec rD nnt) - (Vec.scaleVec n (ddn*nnt+(sqrt cos2t))))
                        else Vec.normVec ((Vec.scaleVec rD nnt) - (Vec.scaleVec n ((-1)*(ddn*nnt+(sqrt cos2t)))))
                    r0 = ((nt-nc)**2)/((nt+nc)**2)
                    c = 1 - (if into then (-1)*ddn else (Vec.dotVec tdir n))

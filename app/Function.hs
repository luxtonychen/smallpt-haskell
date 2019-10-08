module Function where

import qualified Vec
import qualified Shape
import Data.Maybe
import System.Random
import Debug.Trace

radiance :: (RandomGen g) => [Shape.Element] -> Shape.Ray -> g -> Integer -> (Vec.Vec, g)
radiance [] _ randGen _ = ((Vec.Vec 0 0 0), randGen)
radiance scene ray randGen depth
    | (depth > 5) && (randNum >= p) = (selfEmission, randGen1)
    | otherwise = case (t, element) of
        (Nothing, _)    -> ((Vec.Vec 0 0 0), randGen)
        (_, Just (Shape.Sphere _ _ _ _ Shape.Diff)) -> ((Vec.addVec selfEmission (Vec.multVec selfColor nextDiff)), randGen3)
        (_, Just (Shape.Sphere _ _ _ _ Shape.Spec)) -> ((Vec.addVec selfEmission (Vec.multVec selfColor nextSpec)), randGen4)
        (_, Just (Shape.Sphere _ _ _ _ Shape.Refr)) -> ((Vec.addVec selfEmission (Vec.multVec selfColor nextRefr)), randGen5)
    where 
        (t, element) = intersection scene ray 
        selfEmission = if element /= Nothing then Shape.emission (fromJust element) else (Vec.Vec 0 0 0)
        f = if element /= Nothing then Shape.color (fromJust element) else (Vec.Vec 0 0 0)
        p = if (Vec.x f > Vec.y f) && (Vec.x f > Vec.z f) then Vec.x f else (if Vec.y f > Vec.z f then Vec.y f else Vec.z f)
        (randNum, randGen1) = random randGen 
        selfColor = if (depth > 5) && (p /= 0) then Vec.scaleVec f (1/p) else f
        (rayDiff, randGen2) = diffRay element t ray randGen1 
        (nextDiff, randGen3) = radiance scene rayDiff randGen2 (depth+1)
        raySpec = specRay element t ray  
        (nextSpec, randGen4) = radiance scene raySpec randGen1 (depth+1)
        (reRay, trRay, re, tr) = refrRay element t ray 
        (nextRefr, randGen5) = case trRay of 
            Nothing -> radiance scene reRay randGen1 (depth+1)
            _ -> if depth > 2 
                then (if randNum2 < pRerf then ((Vec.scaleVec retFl rp), randGen7) else ((Vec.scaleVec retFr tp), randGen8))
                else ((Vec.addVec (Vec.scaleVec retFl (fromJust re)) (Vec.scaleVec retFr2 (fromJust tr))), randGen9)
                where
                    pRerf = 0.25 + 0.5 * (fromJust re)
                    rp = (fromJust re) / pRerf
                    tp = (fromJust tr) / (1-pRerf)
                    (randNum2, randGen6) = random randGen1
                    (retFl, randGen7) = radiance scene reRay randGen6 (depth+1)
                    (retFr, randGen8) = radiance scene (fromJust trRay) randGen6 (depth+1)
                    (retFr2, randGen9) = radiance scene (fromJust trRay) randGen7 (depth+1)


diffRay :: (RandomGen g) => Maybe Shape.Element -> Maybe Double -> Shape.Ray -> g -> (Shape.Ray, g)
diffRay (Just element) (Just t) (Shape.Ray rO rD) randGen = ( Shape.Ray x (Vec.normVec (Vec.addVec (Vec.addVec u' v') w')), randGen2 )
    where 
        u' = Vec.scaleVec u ((cos r1) * r2s)
        v' = Vec.scaleVec v ((sin r1) * r2s)
        w' = Vec.scaleVec w (sqrt (1 - r2))
        w = nl 
        u = Vec.normVec (Vec.crossVec (if (abs (Vec.x w )) > 0.1 then (Vec.Vec 0 1 0) else (Vec.Vec 1 0 0)) w)
        v = Vec.crossVec w u 
        nl = if (Vec.dotVec n rD) < 0 then n else Vec.scaleVec n (-1)
        n = Vec.normVec (Vec.subVec x (Shape.position element))
        x = Vec.addVec rO (Vec.scaleVec rD t)
        (rnd1, randGen1) = random randGen 
        (r2, randGen2) = random randGen1
        r1 = rnd1*2*pi
        r2s = sqrt r2

specRay :: Maybe Shape.Element -> Maybe Double -> Shape.Ray -> Shape.Ray
specRay (Just element) (Just t) (Shape.Ray rO rD) = Shape.Ray x (Vec.subVec rD (Vec.scaleVec n (2 * (Vec.dotVec n rD))))
    where
        n = Vec.normVec (Vec.subVec x (Shape.position element))
        x = Vec.addVec rO (Vec.scaleVec rD t)

refrRay :: Maybe Shape.Element -> Maybe Double -> Shape.Ray -> (Shape.Ray, Maybe Shape.Ray, Maybe Double, Maybe Double)
refrRay (Just element) (Just t) (Shape.Ray rO rD)
    | cos2t < 0 = ( refl, Nothing, Nothing, Nothing )
    | otherwise = ( refl, Just refr, Just re, Just tr )
    where
        refl = Shape.Ray x (Vec.subVec rD (Vec.scaleVec n (2 * (Vec.dotVec n rD))))
        refr = Shape.Ray x tdir
        re = r0 + (1 - r0)*(c**5)
        tr = 1-re
        into = Vec.dotVec n nl > 0
        nl = if (Vec.dotVec n rD) < 0 then n else Vec.scaleVec n (-1)
        n = Vec.normVec (Vec.subVec x (Shape.position element))
        x = Vec.addVec rO (Vec.scaleVec rD t)
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
        


intersection :: [Shape.Element] -> Shape.Ray -> (Maybe Double, Maybe Shape.Element) 
intersection eleList r = intersection' eleList r Nothing Nothing

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
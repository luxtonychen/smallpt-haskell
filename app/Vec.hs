module Vec where 

data Vec =   Vec {
    x   ::  !Double,
    y   ::  !Double,
    z   ::  !Double 
    }deriving (Eq, Show)

instance Num Vec where
    (Vec x1 y1 z1) + (Vec x2 y2 z2) = Vec (x1+x2) (y1+y2) (z1+z2)
    (Vec x1 y1 z1) - (Vec x2 y2 z2) = Vec (x1-x2) (y1-y2) (z1-z2)
    (Vec x1 y1 z1) * (Vec x2 y2 z2) = Vec (x1*x2) (y1*y2) (z1*z2)
    abs (Vec x1 y1 z1) = Vec (abs x1) (abs y1) (abs z1)
    signum (Vec x1 y1 z1) = Vec (signum x1) (signum y1) (signum z1)
    fromInteger x = Vec (fromInteger x) (fromInteger x) (fromInteger x)

zero = Vec 0 0 0

pairOp :: (Double -> Double -> Double) -> Vec -> Vec -> Vec
pairOp op vec1 vec2 = Vec ( op (x vec1) (x vec2) ) ( op (y vec1) (y vec2) ) ( op (z vec1) (z vec2) )

mapOp :: (Double -> Double -> Double) -> Double -> Vec -> Vec 
mapOp op s vec1 = Vec ( op (x vec1) s ) ( op (y vec1) s ) ( op (z vec1) s )

reduceOp :: (Double -> Double -> Double) -> Vec -> Double
reduceOp op vec1 = op (op (x vec1) (y vec1)) (z vec1)

scaleVec :: Vec -> Double -> Vec
scaleVec vec1 scale = mapOp (*) scale vec1

dotVec :: Vec -> Vec -> Double
dotVec vec1 vec2 = reduceOp (+) (vec1 * vec2)

normVec :: Vec -> Vec 
normVec vec1 = scaleVec vec1 ((/) 1 (sqrt (reduceOp (+) (mapOp (**) 2 vec1))))

crossVec :: Vec -> Vec -> Vec 
crossVec vec1 vec2 = Vec ((y1*z2) - (z1*y2)) ((z1*x2) - (x1*z2)) ((x1*y2) - (y1*x2))
    where 
        x1 = x vec1
        y1 = y vec1 
        z1 = z vec1 
        x2 = x vec2
        y2 = y vec2
        z2 = z vec2

maxDim :: Vec -> Double 
maxDim v = if x v > y v && x v > z v then x v else if y v > z v then y v else z v

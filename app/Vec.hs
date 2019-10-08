module Vec where 

data Vec =   Vec {
    x   ::  Double,
    y   ::  Double,
    z   ::  Double 
    }deriving (Eq, Show)

pairOp :: (Double -> Double -> Double) -> Vec -> Vec -> Vec
pairOp op vec1 vec2 = Vec ( op (x vec1) (x vec2) ) ( op (y vec1) (y vec2) ) ( op (z vec1) (z vec2) )

mapOp :: (Double -> Double -> Double) -> Double -> Vec -> Vec 
mapOp op s vec1 = Vec ( op (x vec1) s ) ( op (y vec1) s ) ( op (z vec1) s )

reduceOp :: (Double -> Double -> Double) -> Vec -> Double
reduceOp op vec1 = op (op (x vec1) (y vec1)) (z vec1)

addVec :: Vec -> Vec -> Vec
addVec vec1 vec2 = pairOp (+) vec1 vec2

subVec :: Vec -> Vec -> Vec
subVec vec1 vec2 = pairOp (-) vec1 vec2

scaleVec :: Vec -> Double -> Vec
scaleVec vec1 scale = mapOp (*) scale vec1

multVec :: Vec -> Vec -> Vec
multVec vec1 vec2 = pairOp (*) vec1 vec2

dotVec :: Vec -> Vec -> Double
dotVec vec1 vec2 = reduceOp (+) (multVec vec1 vec2)

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

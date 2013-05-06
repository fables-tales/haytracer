module Vector3 where

data VectorClass = Vector3 (Double, Double, Double) deriving (Show, Eq)

normalized :: VectorClass -> VectorClass
normalized (Vector3 (x, y, z)) = Vector3 (x/length, y/length, z/length)
    where length = sqrt ( x*x + y*y + z*z)

negative :: VectorClass -> VectorClass
negative (Vector3 (x, y, z)) = Vector3 (-x, -y, -z)

addVectors :: [VectorClass] -> VectorClass
addVectors = foldl addVector (Vector3 (0,0,0))

addVector :: VectorClass -> VectorClass -> VectorClass
addVector (Vector3 (a,b,c)) (Vector3 (d,e,f)) = Vector3 (a+d,b+e,c+f)

subVector :: VectorClass -> VectorClass -> VectorClass
subVector (Vector3 (a,b,c)) (Vector3 (d,e,f)) = Vector3 (a-d,b-e,c-f)

len2 :: VectorClass -> Double
len2 (Vector3 (a,b,c)) = a*a + b*b + c*c

scaled :: VectorClass -> Double -> VectorClass
scaled (Vector3 (a,b,c)) factor = Vector3 (a*factor, b*factor, c*factor)

crossProduct :: VectorClass -> VectorClass -> VectorClass
crossProduct (Vector3 (x1, y1, z1)) (Vector3 (x2, y2, z2)) = Vector3 (y1*z2 - z1*y2, z1*x2 - x1*z2, x1*y2-y1*x2)

dotProduct :: VectorClass -> VectorClass -> Double
dotProduct (Vector3 (a,b,c)) (Vector3 (d,e,f)) = a*d+b*e+c*f

vectorDistance :: VectorClass -> VectorClass -> Double
vectorDistance a b = sqrt (len2 (a `subVector` b))

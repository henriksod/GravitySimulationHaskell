module Math.Vector
( Vector(..)
, zero
, plus
, minus
, dot
, scale
, cross
, dist
, unit
, vecLength
) where

data (Num a) => Vector a = Vector [a] | Vector3 a a a | Vector2 a a
  deriving (Show, Eq)

zero :: (Num a) => Int -> Vector a
zero i = Vector (replicate i 0)

plus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector a) `plus` (Vector b) =  Vector (zipWith (+) a b)
(Vector3 i j k) `plus` (Vector3 l m n) = Vector3 (i+l) (j+m) (k+n)
(Vector2 i j) `plus` (Vector2 l m) = Vector2 (i+l) (j+m)

minus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector a) `minus` (Vector b) =  Vector (zipWith (-) a b)
(Vector3 i j k) `minus` (Vector3 l m n) = Vector3 (i-l) (j-m) (k-n)
(Vector2 i j) `minus` (Vector2 l m) = Vector2 (i-l) (j-m)

dot :: (Num a) => Vector a -> Vector a -> a
(Vector a) `dot` (Vector b) =  sum (zipWith (*) a b)
(Vector3 i j k) `dot` (Vector3 l m n) = i*l + j*m + k*n
(Vector2 i j) `dot` (Vector2 l m) = i*l + j*m

scale :: (Num a, Fractional a) => Vector a -> a -> Vector a
(Vector a) `scale` m =  Vector (fmap (*m) a)
(Vector3 i j k) `scale` m = Vector3 (i*m) (j*m) (k*m)
(Vector2 i j) `scale` m = Vector2 (i*m) (j*m)

cross :: (Num a) => Vector a -> Vector a -> Either (Vector a) a
(Vector _) `cross` (Vector _)           = 0
(Vector2 i j) `cross` (Vector2 l m)     = i*m - j*l
(Vector3 i j k) `cross` (Vector3 l m n) =
    Vector3 (j*n) 0 0
    `plus` Vector3 0 (k*l) 0
    `plus` Vector3 0 0 (i*m)
    `minus` Vector3 0 (i*n) 0
    `minus` Vector3 (k*m) 0 0
    `minus` Vector3 0 0 (j*l)

dist :: (Num a, Floating a) => Vector a -> Vector a -> a
(Vector a) `dist` (Vector b) =  sqrt (sum (fmap (**2) (zipWith (-) b a)))
(Vector3 i j k) `dist` (Vector3 l m n) = sqrt ((l-i)**2 + (j-m)**2 + (n-k)**2)
(Vector2 i j) `dist` (Vector2 l m) = sqrt ((l-i)**2 + (j-m)**2)

unit :: (Num a, Fractional a, Floating a) => Vector a -> Vector a -> Vector a
unit a b = (b `minus` a) `scale` (1/(a `dist` b))

vecLength :: (Num a, Floating a) => Vector a -> a
vecLength (Vector v) = sqrt (sum (fmap (**2) v))

module Math.Matrix
(

) where

  import Math.Vector

  newtype (Num a) => Matrix a = Matrix [Vector a] deriving (Show, Eq)

  

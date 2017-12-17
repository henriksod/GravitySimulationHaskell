module Points where

import Graphics.Rendering.OpenGL

points :: GLfloat -> Int -> [(GLfloat,GLfloat,GLfloat)]
points a n = [ (a*sin (2*pi*k/n'), a*cos (2*pi*k/n'), 0) | k <- [1..n'] ]
   where n' = fromIntegral n

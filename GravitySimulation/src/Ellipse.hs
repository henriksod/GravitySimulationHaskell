module Ellipse where

import Graphics.UI.GLUT
import Points

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

ellipse :: GLfloat -> IO ()
ellipse r = renderPrimitive Polygon $ mapM_ vertex3f
  (points r 10)

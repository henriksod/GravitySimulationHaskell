module Line where

import Graphics.UI.GLUT

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

line :: (GLfloat, GLfloat, GLfloat) -> (GLfloat, GLfloat, GLfloat) -> IO ()
line (x1,y1,z1) (x2,y2,z2) = renderPrimitive Lines $ do
    vertex $ Vertex3 x1 y1 z1
    vertex $ Vertex3 x2 y2 z2

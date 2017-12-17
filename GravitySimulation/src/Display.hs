module Display (idle, display) where

  import Graphics.UI.GLUT
  import Control.Monad
  import Data.IORef
  import Ellipse
  import Line
  import Points
  import Physics.Gravity
  import qualified Math.Vector as V

  display :: IORef [Body] -> IORef (GLfloat,GLfloat) -> IORef GLfloat -> DisplayCallback
  display bodies pos scaleIO = do
    bs <- get bodies
    s <- get scaleIO

    clear [ColorBuffer]
    loadIdentity

    (x',y') <- get pos
    translate $ Vector3 x' y' 0

    preservingMatrix $ do
      scale s s s
      forM_ (getPositions bs) $ \(x,y,z) -> preservingMatrix $ do
        color $ Color3 (massAtPos (x,y,z) bs / 5) 1 1
        translate $ Vector3 x y z
        ellipse (massAtPos (x,y,z) bs)
        line (0,0,0) (vecToTrip (velAtPos (x,y,z) bs
          `V.scale` (1/V.vecLength (velAtPos (x,y,z) bs))
            `V.scale` (massAtPos (x,y,z) bs * 2)))
    swapBuffers

  idle :: IORef [Body] -> IORef GLfloat -> IdleCallback
  idle bodies delta = do
    d <- get delta
    bodies $~! updateBodies 10000000 -- Update position with time scale
    postRedisplay Nothing


------------------------------------------------------------------
  getPositions :: [Body] -> [(GLfloat,GLfloat,GLfloat)]
  getPositions [] = []
  getPositions (Body _ (V.Vector r) _ _:xs) =
    (head r
    ,head $ tail r
    ,0) : getPositions xs

  getPosition :: Body -> Vector3 GLfloat
  getPosition (Body _ (V.Vector r) _ _) = Vector3 (head r) (head $ tail r) 0

  vecToTrip :: V.Vector GLfloat -> (GLfloat,GLfloat,GLfloat)
  vecToTrip (V.Vector r) = (head r, head $ tail r, 0)

  massAtPos :: (GLfloat,GLfloat,GLfloat) -> [Body] -> GLfloat
  massAtPos _ [] = 0
  massAtPos (x,y,z) (Body m (V.Vector r) _ _:bs)
    | x == head r && y == head (tail r) && z == head (tail $ tail r) = m/2
    | otherwise = massAtPos (x,y,z) bs

  velAtPos :: (GLfloat,GLfloat,GLfloat) -> [Body] -> V.Vector GLfloat
  velAtPos _ [] = V.zero 3
  velAtPos (x,y,z) (Body _ (V.Vector r) v _:bs)
    | x == head r && y == head (tail r) && z == head (tail $ tail r) = v
    | otherwise = velAtPos (x,y,z) bs

  accelAtPos :: (GLfloat,GLfloat,GLfloat) -> [Body] -> V.Vector GLfloat
  accelAtPos _ [] = V.zero 3
  accelAtPos (x,y,z) (Body _ (V.Vector r) _ a:bs)
    | x == head r && y == head (tail r) && z == head (tail $ tail r) = a
    | otherwise = accelAtPos (x,y,z) bs

  biggest :: [Body] -> Body
  biggest l = findBiggest l zeroBody

  findBiggest :: [Body] -> Body -> Body
  findBiggest [] b = b
  findBiggest (Body m r v a:xs) (Body bm br bv ba)
    | m > bm = findBiggest xs (Body m r v a)
    | otherwise = findBiggest xs (Body bm br bv ba)

module Main where

import Graphics.UI.GLUT
import Data.IORef
import Bindings
import Physics.Gravity
import Data.Fixed
import qualified Math.Vector as V

newBodies = [Body 0.3 (V.Vector [x, y, 0]) (V.zero 3) (V.zero 3)
            | x <- [-10..10]
            , y <- [-10..10]
            , x `mod'` 2 == 0 && y `mod'` 2 == 0]

testBod = [Body 0.4 (V.Vector [1,2,0]) (V.zero 3) (V.zero 3)
          , Body 0.2 (V.Vector [3,4,0]) (V.zero 3) (V.zero 3)
          , Body 0.3 (V.Vector [2,2,0]) (V.zero 3) (V.zero 3)
          , Body 1 (V.Vector [-2,-3,0]) (V.zero 3) (V.zero 3)]

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Hello World"
  reshapeCallback $= Just reshape
  bodies <- newIORef newBodies
  delta <- newIORef 100
  sc <- newIORef 0.1
  pos <- newIORef (0, 0)
  keyboardMouseCallback $= Just (keyboardMouse bodies sc pos)
  idleCallback $= Just (idle bodies delta)
  displayCallback $= display bodies pos sc
  mainLoop

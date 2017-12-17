module Bindings (idle, display, reshape, keyboardMouse) where

  import Graphics.UI.GLUT
  import Data.IORef
  import Display
  import Physics.Gravity
  import qualified Math.Vector as V

  reshape :: ReshapeCallback
  reshape size = do
    viewport $= (Position 0 0, size)

  keyboardMouse :: IORef [Body] ->IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
  keyboardMouse bs a p key Down _ (Position mx my) = case key of
    (Char '+') -> a $~! (* 2)
    (Char '-') -> a $~! (/ 2)
    (Char ' ') -> p $~! \(x,y) -> (0,0)
    (SpecialKey KeyLeft ) -> p $~! \(x,y) -> (x+0.1,y)
    (SpecialKey KeyRight) -> p $~! \(x,y) -> (x-0.1,y)
    (SpecialKey KeyUp   ) -> p $~! \(x,y) -> (x,y-0.1)
    (SpecialKey KeyDown ) -> p $~! \(x,y) -> (x,y+0.1)
    --(MouseButton LeftButton) -> bs $~! (++ [Body 0.2 (V.Vector [mx, my, 0]) (V.zero 3) (V.zero 3)])
    _ -> return ()
  keyboardMouse _ _ _ _ _ _ _ = return ()

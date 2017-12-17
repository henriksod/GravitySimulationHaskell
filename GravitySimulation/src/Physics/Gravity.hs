module Physics.Gravity
( Body(..)
, zeroBody
, cForce
, cAcceleration
, cVelocity
, cPosition
, updateBodies
, collidingWith
) where

  import Math.Vector
  import Graphics.UI.GLUT (GLfloat)

  data Body = Body {
    mass :: GLfloat,
    position :: Vector GLfloat,
    velocity :: Vector GLfloat,
    acceleration :: Vector GLfloat
  } deriving (Show, Eq)

  _G :: GLfloat
  _G = 6.67408 * 10^^(-11)

  zeroBody :: Body
  zeroBody = Body 0 (zero 3) (zero 3) (zero 3)

  cForce :: Body -> [Body] -> Vector GLfloat
  cForce (Body m r v a) bs = l (Body m r v a) bs `scale` _G
      where
          l _ [] = zero (length bs)
          l (Body m1 r1 v1 a1) (Body m2 r2 v2 a2:b)
              | Body m2 r2 v2 a2 == Body m1 r1 v1 a1 = l (Body m1 r1 v1 a1) b
              | otherwise = (r1 `unit` r2) `scale` ((m1*m2)/((r1 `dist` r2 + 0.1)**2))
                              `plus` l (Body m1 r1 v a) b

  cAcceleration :: Body -> [Body] -> GLfloat -> Body
  cAcceleration (Body m r v a) l dt =
    Body m r v ((cForce (Body m r v a) l `scale` (1/m)) `scale` dt)

  cVelocity :: Body -> Body
  cVelocity (Body m r v a) = Body m r (v `plus` a) a

  cPosition :: Body -> Body
  cPosition (Body m r v a) = Body m (r `plus` v) v a

  nxtVelocity :: Body -> Vector GLfloat
  nxtVelocity (Body m r v a) = v `plus` a

  nxtPosition :: Body -> Vector GLfloat
  nxtPosition (Body m r v a) = r `plus` v

  updateBodies :: GLfloat -> [Body] -> [Body]
  updateBodies _ [] = []
  updateBodies dt l = updateBodies' l l dt

  updateBodies' :: [Body] -> [Body] -> GLfloat -> [Body]
  updateBodies' [] _ _ = []
  updateBodies' (x:xs) l dt =
    cPosition (cVelocity (checkCollision (cAcceleration x l dt) l)):updateBodies' xs l dt

  checkCollision :: Body -> [Body] -> Body
  checkCollision b _ = b
  {-
  checkCollision b [] = b
  checkCollision (Body m r v a) (b:bs)
    | Body m r v a == b = checkCollision (Body m r v a) bs
    | otherwise = if collidingWith (Body m r v a) b then
                    cCollisionForce (Body m r v a) b
                  else
                    checkCollision (Body m r v a) bs
  -}

  collidingWith :: Body -> Body -> Bool
  collidingWith (Body m1 (Vector r1) _ _) (Body m2 (Vector r2) _ _)
    | bd x1 x2 m2 && bd y1 y2 m2 = True
    | bd x2 x1 m1 && bd y2 y1 m1  = True
    | otherwise = False
    where
      bd a b m = a <= b + m*10 && a >= b + m*10
      x1 = head r1
      x2 = head r2
      y1 = head $ tail r1
      y2 = head $ tail r2

  cCollisionForce :: Body -> Body -> Body
  cCollisionForce (Body m1 r1 v1 a1) (Body m2 r2 v2 a2) =
    Body m1 r1 v1 (((r2 `unit` r1) `minus` eq) `scale` m2)
      where
        eq = n `scale` (((r2 `unit` r1) `dot` n) * 2.0)
          where
            n = v1 `scale` (1/vecLength v1)

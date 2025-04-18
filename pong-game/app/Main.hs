module Main where

import Raylib.Core
import Raylib.Util (WindowResources)
import Raylib.Util.Colors (black, red, white)
import Raylib.Core.Shapes (drawRectangle, drawCircle)
import Raylib.Types (KeyboardKey(KeyRight, KeyLeft, KeyD, KeyA))

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

paddleWidth, paddleHeight :: Int
paddleWidth = 100
paddleHeight = 15

paddleVelocity :: Float
paddleVelocity = 10

ballRadius :: Float
ballRadius = 5.0

ballVelocity :: Float
ballVelocity = 8.0

type Vec2 = (Float, Float)
type Paddle = Vec2

data Ball = Ball
  { position :: Vec2
  , velocity :: Vec2
  }

data Game = Game
  { player1 :: Paddle
  , player2 :: Paddle
  , ball    :: Ball
  }

initialGame :: Game
initialGame =
  Game p1 p2 b
  where
    centerX = fromIntegral windowWidth / 2
    centerY = fromIntegral windowHeight / 2
    b  = Ball (centerX, centerY) (ballVelocity, ballVelocity)
    p1 = (centerX - fromIntegral paddleWidth / 2, 20)
    p2 = (centerX - fromIntegral paddleWidth / 2, fromIntegral windowHeight - fromIntegral paddleHeight - 20)

main :: IO ()
main = do
  window <- initWindow windowWidth windowHeight "Pong Game"
  setTargetFPS 60
  gameLoop window initialGame

gameLoop :: WindowResources -> Game -> IO ()
gameLoop window game = do
  shouldClose <- windowShouldClose
  if shouldClose
    then closeWindow (Just window)
    else do
      beginDrawing
      clearBackground white
      drawGame game
      endDrawing

      inputGame <- handleInput game
      let newBall = updateBall (ball inputGame) (player1 inputGame) (player2 inputGame)
      gameLoop window inputGame { ball = newBall }

drawGame :: Game -> IO ()
drawGame (Game (x1, y1) (x2, y2) (Ball (bx, by) _)) = do
  drawRectangle (round x1) (round y1) paddleWidth paddleHeight black
  drawRectangle (round x2) (round y2) paddleWidth paddleHeight black
  drawCircle (round bx) (round by) ballRadius red

handleInput :: Game -> IO Game
handleInput game = do
  keyRight <- isKeyDown KeyRight
  keyLeft  <- isKeyDown KeyLeft
  keyD     <- isKeyDown KeyD
  keyA     <- isKeyDown KeyA

  let limitX x = max 0 (min (fromIntegral windowWidth - fromIntegral paddleWidth) x)

      (x1, y1) = player1 game
      newX1
        | keyRight = limitX (x1 + paddleVelocity)
        | keyLeft  = limitX (x1 - paddleVelocity)
        | otherwise = x1

      (x2, y2) = player2 game
      newX2
        | keyD = limitX (x2 + paddleVelocity)
        | keyA = limitX (x2 - paddleVelocity)
        | otherwise = x2

  return game { player1 = (newX1, y1), player2 = (newX2, y2) }

updateBall :: Ball -> Paddle -> Paddle -> Ball
updateBall (Ball (x, y) (vx, vy)) p1 p2 =
  let nextX = x + vx
      nextY = y + vy

      bouncedVX
        | nextX <= ballRadius || nextX >= fromIntegral windowWidth - ballRadius = -vx
        | otherwise = vx

      tempBall = Ball (nextX, nextY) (bouncedVX, vy)

      bouncedVY
        | checkPaddleCollision tempBall p1 = abs vy
        | checkPaddleCollision tempBall p2 = -abs vy
        | nextY <= ballRadius || nextY >= fromIntegral windowHeight - ballRadius = -vy
        | otherwise = vy

  in Ball (nextX, nextY) (bouncedVX, bouncedVY)

checkPaddleCollision :: Ball -> Paddle -> Bool
checkPaddleCollision (Ball (bx, by) _) (px, py) =
  let withinX = bx + ballRadius >= px && bx - ballRadius <= px + fromIntegral paddleWidth
      withinY = by + ballRadius >= py && by - ballRadius <= py + fromIntegral paddleHeight
  in withinX && withinY

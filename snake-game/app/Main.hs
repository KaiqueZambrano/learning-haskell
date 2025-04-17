module Main where

import Raylib.Core
import Raylib.Util (WindowResources)
import Raylib.Util.Colors (green, red, white)
import Raylib.Core.Shapes (drawRectangle, checkCollisionRecs)
import Raylib.Types (KeyboardKey(..), Rectangle(..))

cellSize :: Int
cellSize = 10

screenWidth, screenHeight :: Int
screenWidth = 800
screenHeight = 600

gridWidth, gridHeight :: Int
gridWidth = screenWidth `div` cellSize
gridHeight = screenHeight `div` cellSize

data Direction = UP | DOWN | RIGHT | LEFT deriving Eq
type Point = (Int, Int)
type Snake = [Point]
type Food  = Point

data Game = Game
  { snake     :: Snake
  , food      :: Food
  , direction :: Direction
  }

main :: IO ()
main = do
  window <- initWindow screenWidth screenHeight "Snake Game"
  setTargetFPS 15

  let initialGame = Game
        { snake = [(screenWidth `div` 2, screenHeight `div` 2)]
        , food = (200, 200)
        , direction = UP
        }

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

      newDir <- receiveInput (direction game)
      let newHead  = moveHead (head (snake game)) newDir
          hasEaten = foodCollision newHead (food game)
          oldBody  = snake game
          collided = selfCollision newHead (tail oldBody)

      if collided
        then closeWindow (Just window)
        else do
          newFood <- if hasEaten then generateRandomFood else return (food game)
          let newSnake = newHead : if hasEaten then oldBody else init oldBody

          let newGame = Game
                { snake = newSnake
                , food = newFood
                , direction = newDir
                }

          gameLoop window newGame

receiveInput :: Direction -> IO Direction
receiveInput dir = do
  up    <- isKeyDown KeyUp
  down  <- isKeyDown KeyDown
  right <- isKeyDown KeyRight
  left  <- isKeyDown KeyLeft

  return $ case () of
    _ | up    && dir /= DOWN -> UP
      | down  && dir /= UP   -> DOWN
      | right && dir /= LEFT -> RIGHT
      | left  && dir /= RIGHT -> LEFT
      | otherwise             -> dir

moveHead :: Point -> Direction -> Point
moveHead (x, y) UP    = (x, y - cellSize)
moveHead (x, y) DOWN  = (x, y + cellSize)
moveHead (x, y) RIGHT = (x + cellSize, y)
moveHead (x, y) LEFT  = (x - cellSize, y)

generateRandomFood :: IO Food
generateRandomFood = do
  x <- getRandomValue 0 (gridWidth - 1)
  y <- getRandomValue 0 (gridHeight - 1)
  return (x * cellSize, y * cellSize)

foodCollision :: Point -> Point -> Bool
foodCollision snakeHead foodPos =
  checkCollisionRecs (pointToRect snakeHead) (pointToRect foodPos)

selfCollision :: Point -> Snake -> Bool
selfCollision headPos body = headPos `elem` body

drawGame :: Game -> IO ()
drawGame Game{snake = s, food = f} = do
  mapM_ (\(x, y) -> drawRectangle x y cellSize cellSize green) s
  let (fx, fy) = f
  drawRectangle fx fy cellSize cellSize red

pointToRect :: Point -> Rectangle
pointToRect (x, y) =
  Rectangle (fromIntegral x) (fromIntegral y)
            (fromIntegral cellSize) (fromIntegral cellSize)

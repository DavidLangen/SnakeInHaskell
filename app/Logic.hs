module Logic where

import Config
import Game
import Graphics.Gloss.Interface.Pure.Game

transformGame :: Event -> Game -> Game
transformGame = handleKeys

handleKeys :: Event -> Game -> Game
handleKeys (EventKey (Char 'w') Down _ _) game = moveSnake UP game
handleKeys (EventKey (Char 'a') Down _ _) game = moveSnake LEFT game
handleKeys (EventKey (Char 'd') Down _ _) game = moveSnake RIGHT game
handleKeys (EventKey (Char 's') Down _ _) game = moveSnake DOWN game
handleKeys _ game = game

updateOverTime :: Float -> Game -> Game
updateOverTime time game = translateSnakeHeadWithSpeed directionOfPlayer speed game
  where
    speed = truncate (snakeSpeed + time)
    directionOfPlayer = direction (gamePlayer game)

vectorByDirection :: Direction -> Int -> (Int, Int)
vectorByDirection d speed = case d of
  UP -> (speed, 0)
  DOWN -> (- speed, 0)
  LEFT -> (0, - speed)
  RIGHT -> (0, speed)

moveSnake :: Direction -> Game -> Game
moveSnake
  d
  game@Game
    { gamePlayer = Player {direction = playerD}
    }
    | checkOppositeDirection d playerD = game
    | otherwise = translateSnakeHeadWithSpeed d 1 game

getOppositeDirectionOf :: Direction -> Direction
getOppositeDirectionOf d = case d of
  UP -> DOWN
  DOWN -> UP
  LEFT -> RIGHT
  RIGHT -> LEFT

checkOppositeDirection :: Direction -> Direction -> Bool
checkOppositeDirection a b = a == getOppositeDirectionOf b

checkWallAndTailCollison :: Snake -> Bool
checkWallAndTailCollison s = checkWallHeadCollison (head s) || head s `elem` tail s

checkWallHeadCollison :: Cell -> Bool
checkWallHeadCollison (x,y) = x >= amountOfCells || y >= amountOfCells || x < 0 || y < 0

-- vielleicht mit record matching
translateSnakeHeadWithSpeed :: Direction -> Int -> Game -> Game
translateSnakeHeadWithSpeed d speed game
  | checkWallAndTailCollison newSnake = game {gameState = GameOver}
  | otherwise = updateSnakeAndDirection game newSnake d
  where
    sn = snake (gamePlayer game)
    withLastElem = head sn `elem` (gameBoard game)
    newSnake = updateSnake withLastElem (vectorByDirection d speed) sn

updateSnake :: Bool -> (Int, Int) -> Snake -> Snake
updateSnake _ _ [] = []
updateSnake _ (amountX, amountY) [(x, y)] = [(amountX + x, amountY + y)]
updateSnake withLastElem (amountX, amountY) (h@(x, y) : xs) = (amountX + x, amountY + y) : h : take (length xs - amountOfLatestElem) xs
                                                    where amountOfLatestElem = if withLastElem then 0 else 1

updateSnakeAndDirection :: Game -> Snake -> Direction -> Game
updateSnakeAndDirection game s d = game {gamePlayer = (gamePlayer game) {snake = s, direction = d}}

module Logic where
import Config
import Lib
import Game
import System.Random
import Graphics.Gloss.Interface.Pure.Game

transformGame :: Event -> Game -> Game
transformGame e game = case (gameState game) of
                  Running -> handleGameplayKeys e game
                  GameOver -> handleGameOverKeys e game

handleGameOverKeys :: Event -> Game -> Game
handleGameOverKeys _ game = game {
                                    gameBoard =  [(5,5)],
                                    gamePlayer = Player {snake = [(1,1), (1,2)], direction = UP},
                                    gameState = Running
                                  }



handleGameplayKeys :: Event -> Game -> Game
handleGameplayKeys (EventKey (Char 'w') Down _ _) game = moveSnake UP game
handleGameplayKeys (EventKey (Char 'a') Down _ _) game = moveSnake LEFT game
handleGameplayKeys (EventKey (Char 'd') Down _ _) game = moveSnake RIGHT game
handleGameplayKeys (EventKey (Char 's') Down _ _) game = moveSnake DOWN game
handleGameplayKeys (EventKey (SpecialKey KeyUp) Down _ _) game = moveSnake UP game
handleGameplayKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = moveSnake LEFT game
handleGameplayKeys (EventKey (SpecialKey KeyRight) Down _ _) game = moveSnake RIGHT game
handleGameplayKeys (EventKey (SpecialKey KeyDown) Down _ _) game = moveSnake DOWN game
handleGameplayKeys _ game = game


generateFruitOnFreeCell :: Int -> Snake -> Cell
generateFruitOnFreeCell seed sn = head $ filter (`notElem` sn) $ genRanTuple seed (amountOfCells-1)

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

checkWallAndTailCollision :: Snake -> Bool
checkWallAndTailCollision s = checkWallHeadCollision (head s) || head s `elem` tail s

checkWallHeadCollision :: Cell -> Bool
checkWallHeadCollision (x, y) = x >= amountOfCells || y >= amountOfCells || x < 0 || y < 0

-- vielleicht mit record matching
translateSnakeHeadWithSpeed :: Direction -> Int -> Game -> Game
translateSnakeHeadWithSpeed direct speed game
  | checkWallAndTailCollision newSnake = game {gameState = GameOver}
  | otherwise =
    game
      {
        gameBoard = if collideWithFruit then updateBoardOnCollision board sn newRndInt else board,
        rndGen = newRndGen,
        gamePlayer =
          Player
            { snake = newSnake,
              direction = direct
            }
      }
  where
    sn = snake (gamePlayer game)
    (newRndInt, newRndGen) = next (rndGen game)
    board = gameBoard game
    collideWithFruit = head sn `elem` gameBoard game
    newSnake = updateSnake collideWithFruit (vectorByDirection direct speed) sn

updateBoardOnCollision :: Board -> Snake -> Int -> Board
updateBoardOnCollision board sn rnd = generateFruitOnFreeCell rnd sn : remove fruit board
            where fruit = head sn

updateSnake :: Bool -> (Int, Int) -> Snake -> Snake
updateSnake _ _ [] = []
updateSnake _ (amountX, amountY) [(x, y)] = [(amountX + x, amountY + y)]
updateSnake withLastElem (amountX, amountY) (h@(x, y) : xs) = (amountX + x, amountY + y) : h : take (length xs - amountOfLatestElem) xs
  where
    amountOfLatestElem = if withLastElem then 0 else 1


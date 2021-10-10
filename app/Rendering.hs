module Rendering where

import Config
import Graphics.Gloss
import Game

boardGrid :: Picture
boardGrid = color gridColor (pictures $ concatMap (\index -> [
                                         line [(index * cellWidth, 0.0), (index * cellWidth, fromIntegral screenHeight)],  --vertical lines
                                         line [(0.0, index * cellHeight), (fromIntegral screenWidth, index * cellHeight)]]) -- horizontal lines
                                          [0.0..fromIntegral amountOfCells])

snapPictureToCell :: (Integral a, Integral b) => Picture -> (a, b) -> Picture
snapPictureToCell pic (row, column) = translate x y pic
                                where x = fromIntegral column * cellWidth + cellWidth * 0.5
                                      y = fromIntegral row * cellHeight + cellHeight * 0.5

drawFruit :: Picture
drawFruit = color fruitColor $ circleSolid radius
        where radius = (cellWidth/2) * 0.5 -- better looking

fruitCellsOfBoard :: Board -> Picture
fruitCellsOfBoard fruits = pictures $ map (snapPictureToCell drawFruit) fruits

boardAsPicture :: Snake -> Board -> Picture
boardAsPicture player board  = pictures [boardGrid,
                                  fruitCellsOfBoard board,
                                  snakeCellsOfBoard player]

drawSnakeSingleFragment :: Picture
drawSnakeSingleFragment = color gridColor $ rectangleSolid cellWidth cellHeight

snakeCellsOfBoard :: Snake -> Picture
snakeCellsOfBoard player = pictures $ map (snapPictureToCell drawSnakeSingleFragment) player

--TODO use later a bitmap, because rendering text in gloss is garbage
showGameOverScreen :: Picture
showGameOverScreen = let
                        gameOverTitle = color gameOverTextColor
                                          $ translate (fromIntegral screenWidth * 0.2) (fromIntegral screenHeight * 0.5)
                                          $ scale (0.5) (0.5) $ text "Game Over"
                        infoText = color gameOverTextColor
                                          $ translate (fromIntegral screenWidth * 0.2) (fromIntegral screenHeight * 0.3)
                                          $ scale (0.2) (0.2) $ text "Press any Key to try again!"
                      in
                      pictures [gameOverTitle, infoText]

snakeTitle :: Picture
snakeTitle = color gameOverTextColor
                              $ translate (fromIntegral screenWidth * (- 0.1)) (fromIntegral screenHeight * (1.2))
                              $ scale (0.5) (0.5) $ text "The Snake Game"

gameAsPicture :: Game -> Picture
gameAsPicture
  Game
    { gamePlayer = Player {snake = sn},
      gameBoard = b,
      gameState = state
    } = translateOriginToLeftUpperCorner frame
    where
      frame = case state of
        Running -> pictures [snakeTitle, boardAsPicture sn b]
        GameOver -> showGameOverScreen


translateOriginToLeftUpperCorner :: Picture -> Picture
translateOriginToLeftUpperCorner = translate (fromIntegral screenWidth * (- 0.5)) (fromIntegral screenHeight * (- 0.5))
module Rendering where

import Config
import Graphics.Gloss
import Game

boardGrid :: Picture
boardGrid = color gridColor (pictures $ concatMap (\index -> [
                                         line [(index * cellWidth, 0.0), (index * cellWidth, fromIntegral screenHeight)],  --vertical lines
                                         line [(0.0, index * cellHeight), (fromIntegral screenWidth, index * cellHeight)]]) -- horizontal lines
                                          [0.0..fromIntegral amountOfCells])

boardAsPicture :: Picture
boardAsPicture = pictures [boardGrid]

updateBoardForRunning :: Board -> Picture
updateBoardForRunning board = boardAsPicture

--TODO use later a bitmap, because rendering text in gloss is garbage
showGameOverScreen :: Picture
showGameOverScreen = color gameOverTextColor
                                          $ translate (fromIntegral screenWidth * 0.2) (fromIntegral screenHeight * 0.5)
                                          $ scale (0.5) (0.5) $ text "Game Over"


gameAsPicture :: Game -> Picture
gameAsPicture game =  translateOriginToLeftUpperCorner frame
  where frame = case gameState game of
                      Running -> updateBoardForRunning (gameBoard game)
                      GameOver -> showGameOverScreen

translateOriginToLeftUpperCorner :: Picture -> Picture
translateOriginToLeftUpperCorner = translate (fromIntegral screenWidth * (- 0.5)) (fromIntegral screenHeight * (- 0.5))
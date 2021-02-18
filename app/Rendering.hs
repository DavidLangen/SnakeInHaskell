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


boardAsPicture :: Player -> Picture
boardAsPicture player = pictures [snakeCellsOfBoard player,
                          boardGrid]

drawSnakeSingleFragment :: Picture
drawSnakeSingleFragment = color gridColor $ rectangleSolid cellWidth cellHeight

snakeCellsOfBoard :: Player -> Picture
snakeCellsOfBoard player = pictures $ map (snapPictureToCell drawSnakeSingleFragment) $ player


updateBoardForRunning :: Player -> Picture
updateBoardForRunning player = boardAsPicture player

--TODO use later a bitmap, because rendering text in gloss is garbage
showGameOverScreen :: Picture
showGameOverScreen = color gameOverTextColor
                                          $ translate (fromIntegral screenWidth * 0.2) (fromIntegral screenHeight * 0.5)
                                          $ scale (0.5) (0.5) $ text "Game Over"


gameAsPicture :: Game -> Picture
gameAsPicture game =  translateOriginToLeftUpperCorner frame
  where frame = case gameState game of
                      Running -> updateBoardForRunning (gamePlayer game)
                      GameOver -> showGameOverScreen

translateOriginToLeftUpperCorner :: Picture -> Picture
translateOriginToLeftUpperCorner = translate (fromIntegral screenWidth * (- 0.5)) (fromIntegral screenHeight * (- 0.5))
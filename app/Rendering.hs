module Rendering where

import Graphics.Gloss
import Game


updateBoardForRunning board = Blank
updateBoardForGameOver board = Blank

gameAsPicture :: Game -> Picture
gameAsPicture game =
    case gameState game of
      Running -> updateBoardForRunning (gameBoard game)
      GameOver -> updateBoardForGameOver (gameBoard game)
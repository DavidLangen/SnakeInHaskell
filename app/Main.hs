module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Game
import Logic
import Rendering
window = InWindow "Snake" (540, 480) (100, 100)

backgroundColor = makeColor 0 0 0 255


main :: IO ()
main = play window backgroundColor 30 initalGame gameAsPicture transformGame (const id)
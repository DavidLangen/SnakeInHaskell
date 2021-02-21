module Main where
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment

import Lib
import Animation
import Game
import Logic
import Rendering
import Config


window :: Int -> Int -> Display
window windowPosX windowPosY = InWindow "Snake" (screenWidth, screenHeight) (windowPosX, windowPosY)

main :: IO ()
main = do
        g <- newStdGen
        windowSize <-  fmap (mapTuple (\x -> floor $ fromIntegral x * (-0.5))) getScreenSize   
        play (window (fst windowSize) (snd windowSize)) backgroundColor 2 (initalGame g) gameAsPicture transformGame updateOverTime
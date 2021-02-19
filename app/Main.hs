module Main where
import Lib
import Graphics.Gloss
import Game
import Logic
import Rendering
import Config
import Graphics.Gloss.Interface.Environment


window :: Int -> Int -> Display
window windowPosX windowPosY = InWindow "Snake" (screenWidth, screenHeight) (windowPosX, windowPosY)

main :: IO ()
main = do
        windowSize <-  fmap (mapTuple (\x -> floor $ fromIntegral x * (-0.5))) getScreenSize   
        play (window (fst windowSize) (snd windowSize)) backgroundColor 2 initalGame gameAsPicture transformGame updateOverTime
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Graphics.Gloss
import Game
import Logic
import Rendering
import Config
import Graphics.Gloss.Interface.Environment


window :: Int -> Int -> Display
window windowPosX windowPosY = InWindow "Snake" (screenWidth, screenHeight) (windowPosX, windowPosY)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)


main :: IO ()
main = do
        windowSize <-  fmap (mapTuple (\x -> floor $ fromIntegral x * (-0.5))) getScreenSize   
        play (window (fst windowSize) (snd windowSize)) backgroundColor 30 initalGame gameAsPicture transformGame (const id)
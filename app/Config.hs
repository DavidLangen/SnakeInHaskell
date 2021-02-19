module Config where
import Graphics.Gloss.Data.Color

-- Player
snakeSpeed :: Float
snakeSpeed = 0.5

-- Basic Configs
amountOfCells :: Int
amountOfCells = 10

screenWidth :: Int
screenWidth = 500

screenHeight :: Int
screenHeight = 500

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral amountOfCells

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral amountOfCells

-- Design / Color Configs

backgroundColor :: Color
backgroundColor = makeColor 0 0 0 255

gridColor :: Color
gridColor = makeColor 150 150 150 255

fruitColor :: Color
fruitColor = makeColor 0 0 255 255

gameOverTextColor :: Color
gameOverTextColor = makeColor 150 150 150 255
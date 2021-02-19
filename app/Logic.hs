module Logic where
import Game
import Config
import Graphics.Gloss.Interface.Pure.Game
transformGame :: Event -> Game -> Game
transformGame = handleKeys


handleKeys :: Event -> Game -> Game
handleKeys (EventKey (Char 'w') _ _ _) game = translateSnakeHead UP game
handleKeys (EventKey (Char 'a') _ _ _) game = translateSnakeHead LEFT game
handleKeys (EventKey (Char 'd') _ _ _) game = translateSnakeHead RIGHT game
handleKeys (EventKey (Char 's') _ _ _) game = translateSnakeHead DOWN game
handleKeys _ game = game

updateOverTime :: Float -> Game -> Game
updateOverTime time game = translateSnakeHeadWithSpeed directionOfPlayer speed game
              where speed = truncate (snakeSpeed + time)
                    directionOfPlayer = direction (gamePlayer game)
                    
vectorByDirection :: Direction -> Int -> (Int, Int)
vectorByDirection d speed = case d of
                      UP   ->  (speed , 0)
                      DOWN ->  (-speed, 0)
                      LEFT ->  (0, -speed)
                      RIGHT -> (0, speed)
translateSnakeHead :: Direction -> Game -> Game
translateSnakeHead d = translateSnakeHeadWithSpeed d 1

-- vielleicht mit record matching
translateSnakeHeadWithSpeed :: Direction -> Int -> Game -> Game
translateSnakeHeadWithSpeed d speed game = updateSnakeAndDirection game (updateSnakeHead (vectorByDirection d speed) (snake (gamePlayer game))) d


updateSnakeHead :: (Int, Int) -> Snake -> Snake
updateSnakeHead _ [] = []
updateSnakeHead (amountX, amountY) [(x,y)] = [(amountX + x, amountY + y)]
updateSnakeHead (amountX, amountY) (h@(x,y):xs) = (amountX + x, amountY + y) : h : take (length xs - 1) xs
                

updateSnakeAndDirection :: Game -> Snake -> Direction -> Game
updateSnakeAndDirection game s d = game {gamePlayer = (gamePlayer game) {snake = s, direction = d}}




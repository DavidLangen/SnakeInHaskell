module Animation where
import Config
import Game
import Logic
  
  
updateOverTime :: Float -> Game -> Game
updateOverTime time game = translateSnakeHeadWithSpeed directionOfPlayer speed game
  where
    speed = truncate (snakeSpeed + time)
    directionOfPlayer = direction (gamePlayer game)
module Logic where
import Game
import Graphics.Gloss.Interface.Pure.Game
transformGame :: Event -> Game -> Game
transformGame = handleKeys


handleKeys :: Event -> Game -> Game
handleKeys (EventKey (Char 'w') _ _ _) game = translatePlayerHead (1,  0) game
handleKeys (EventKey (Char 'a') _ _ _) game = translatePlayerHead (0, -1) game
handleKeys (EventKey (Char 'd') _ _ _) game = translatePlayerHead (0,  1) game
handleKeys (EventKey (Char 's') _ _ _) game = translatePlayerHead (-1, 0) game
handleKeys _ game = game

-- vielleicht mit record matching
translatePlayerHead :: (Int, Int) -> Game -> Game
translatePlayerHead amount game = updatePlayer game (updateSnakeHead amount (gamePlayer game))

updateSnakeHead :: (Int, Int) -> Player -> Player
updateSnakeHead (amountX, amountY) (head@(x,y):xs) = (amountX + x, amountY + y) : head : take (length xs - 1) xs

updatePlayer :: Game -> Player -> Game
updatePlayer game player = game { gamePlayer = player}
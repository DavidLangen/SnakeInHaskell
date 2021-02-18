module Game where


type Player = [Cell]
data State = Running | GameOver deriving (Eq, Show)
type Cell = (Int, Int)
newtype Board = Board [Cell] deriving (Eq, Show)
data Game = Game {
                  gameBoard :: Board,
                  gamePlayer :: Player,
                  gameState :: State
                  } deriving (Eq, Show)

initalGame :: Game
initalGame = Game {
gameBoard =  Board (zip [0.. 10] [0..10]),
gamePlayer = [(1,1)],
gameState = Running
}

module Game where


newtype Player = Player [Cell] deriving (Eq, Show)
data State = Running | GameOver deriving (Eq, Show)
data Cell = Empty | Full (Int, Int) deriving (Eq, Show)
newtype Board = Board [Cell] deriving (Eq, Show)
data Game = Game {
                  gameBoard :: Board,
                  gamePlayer :: Player,
                  gameState :: State
                  } deriving (Eq, Show)

initalGame = Game {
gameBoard =  Board (map Full $ zip [0.. 10] [0..10]),
gamePlayer = Player [],
gameState = Running
}

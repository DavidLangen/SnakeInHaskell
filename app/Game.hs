module Game where
import System.Random
import Data.Data

data Player = Player {
                    snake :: Snake,
                    direction :: Direction
                    } deriving (Eq, Show)
type Snake = [Cell]

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Show)
data State = Running | GameOver deriving (Eq, Show)
type Cell = (Int, Int)
type Board = [Cell]
data Game = Game {
                  rndGen :: StdGen,
                  gameBoard :: Board,
                  gamePlayer :: Player,
                  gameState :: State
                  }

initializeGame :: StdGen -> Game
initializeGame g = Game {
rndGen = g,
gameBoard =  [(5,5)],
gamePlayer = Player {snake = [(1,1), (1,2)], direction = UP},
gameState = Running
}

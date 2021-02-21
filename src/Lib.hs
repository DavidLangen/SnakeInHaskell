module Lib  where
import System.Random
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

remove :: Eq a => a -> [a] -> [a]
remove element = filter (\ e -> e /= element)


ran :: Int -> Int -> [Int]
ran seed n = randomRs (0,n) (mkStdGen seed)

genRanTuple :: Int -> Int -> [(Int, Int)]
genRanTuple seed n = zip (ran seed n) (ran seed n)


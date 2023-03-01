module Exercise02 where

import Data.List
import Data.Function
import Data.Ord

{-The first game is “Guess 2/3 of the average”-}
twoThirdsAverageWinners :: [(String, Int)] -> [String]
twoThirdsAverageWinners gs = [fst r | r <- [(fst g, abs (snd g - twoThirdsAverage (snd (unzip gs)))) | g <- gs ], 
                                snd r == minimum [abs (snd g - twoThirdsAverage (snd (unzip gs))) | g <- gs ]]

twoThirdsAverage :: [Int] -> Int
twoThirdsAverage arrayNumbers = (2 * sum arrayNumbers) `div` (length arrayNumbers * 3)

{-The second game is “Unique bid auction” which is an auction with a spin -}
lowestUniqueBidder :: [(String, Int)] -> String
lowestUniqueBidder bs = findLowestUniqueBid (sortBy (compare `on` snd) bs)

findLowestUniqueBid :: [(String, Int)] -> String
findLowestUniqueBid bs
    | length bs == 0 = "Nobody"
    | length bs == 1 = fst (head bs)
    | otherwise = if snd (bs !! 0) == snd (bs !! 1) 
                    then findLowestUniqueBid [g | g <- bs, snd g > snd (bs!!0)] else fst (head bs)




{-The tournament mode is a round robin: each of the players (labelled 1 to n) plays against 
each other player exactly once (no ties). The result is a tournament graph, in which an edge i → j indicates that player i defeated player j.-}

-- return the shortest list in a list of lists
shortest :: [[Int]] -> [Int]
shortest = minimumBy (comparing length)

-- return the set of all players in a tournament
players :: [[Int]] -> [Int]
players tournament = [1..length tournament]

-- return the dominion of player i
dominion :: [[Int]] -> Int -> [Int]
dominion tournament i = tournament !! (i - 1)


{-The dominators of i are the set of all players who defeated i -}
dominators :: [[Int]] -> Int -> [Int]
dominators tournament i = [ x | x <- [1..length tournament], x `notElem` ((dominion tournament i) ++ [i])]

{- i covers j if i defeated everyone that j defeated.-}
covers :: [[Int]] -> Int -> Int-> Bool
covers tournament i j = length [ x | x <- (dominion tournament j), x `notElem` (dominion tournament i)] == 0

{-A set of players X is called dominant if it is non-empty and every player in it defeated every player not in it -}
dominant :: [[Int]] -> [Int] -> Bool
dominant tournament xs 
    | length xs == 0 = False
    | otherwise = length [ y | y <-[x | x <- [1..length tournament], x `notElem` xs], not (length [z | z <- xs, z `elem` dominion tournament y] == 0)] == 0

{- The set of those players who defeated the most other players, i.e. who have the maximum dominion size -}
copeland :: [[Int]] -> [Int]
copeland tournament = [ y | y <- [1..length tournament], length (dominion tournament y) == maximum [length (dominion tournament x) | x <- [1..length tournament]]]

{- The set of all players that are covered by no one -}
uncoveredSet :: [[Int]] -> [Int]
uncoveredSet tournament = undefined
       
{- The smallest dominant set -} 
topCycle :: [[Int]] -> [Int]
topCycle tournament = undefined

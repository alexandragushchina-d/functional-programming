module Exercise03 where

import Text.Printf (printf)
import Data.List (intercalate)

-- a backtracking based Sudoku solver

-- return the nth row of the Sudoku (index starts at 0)
selectRow :: [[Int]] -> Int -> [Int]
selectRow xss i = xss!!i

-- return the nth column of the Sudoku (index starts at 0)
selectColumn :: [[Int]] -> Int -> [Int]
selectColumn xss i = [x!!i | x <- xss]

-- the function intRoot from the template to obtain the square root of an Int
intRoot :: Int -> Int
intRoot = floor . sqrt . fromIntegral

--return numbers in square as a list. squares are numbered  from left to right and top to bottom
--e.g. :
--[0,1,2]
--[3,4,5]
--[6,7,8]
selectSquare :: [[Int]] -> Int -> [Int]
selectSquare xss i = [ xss!!k!!l | k <- [(i `div` squareRootLength xss) * squareRootLength xss..(i `div` squareRootLength xss) * squareRootLength xss + squareRootLength xss - 1],
                                   l <- [(i `mod` squareRootLength xss) *squareRootLength xss ..(i `mod` squareRootLength xss) * squareRootLength xss + squareRootLength xss - 1]]


squareRootLength :: [[Int]] -> Int
squareRootLength xss = intRoot (length xss)

-- check if a given row, column, or square follows the rules
isValidSubsection :: [Int] -> Bool
isValidSubsection [] = True
isValidSubsection (x:xs) = if x `elem` xs && not (x == 0) then False else isValidSubsection xs

isValidSudoku :: [[Int]] -> Bool
isValidSudoku xss = length [i | i <- [0..length xss - 1], isValidSubsection (selectRow xss i) 
                                        && isValidSubsection (selectColumn xss i)
                                        && isValidSubsection (selectSquare xss i)] == length xss

-- return a Sudoku with the cell at position (x,y) set to n
setCell :: [[Int]] -> (Int,Int) -> Int -> [[Int]]
setCell xss (j, k) x = [[ if i == j && l == k then x else xss!!i!!l | l <- [0..length xss - 1]] | i <- [0..length xss - 1]] 

-- return a solution for a given Sudoku
solveSudoku :: [[Int]] -> [[Int]]
solveSudoku xss
  | not (isValidSudoku xss) = []
  | otherwise = if length (getEmptyCells2 xss 0 0) == 0 
                then xss
                else findFirstNotEmpty [solveSudoku (setCell xss ((getEmptyCells2 xss 0 0)!!0) i) | i <- [1..length xss]]
                     
findFirstNotEmpty :: [[[Int]]] -> [[Int]]
findFirstNotEmpty [] = []
findFirstNotEmpty (x:xs) = if length x == 0 then findFirstNotEmpty xs else x

getEmptyCells :: [[Int]] -> [(Int, Int)]
getEmptyCells xss = [(i, j) | i <- [0..length xss - 1], j <- [0..length xss -1], xss!!i!!j == 0]

getEmptyCells2 :: [[Int]] -> Int -> Int -> [(Int, Int)]
getEmptyCells2 xss i j 
  | i == length xss = []
  | xss !! i !! j == 0 = [(i, j)]
  | otherwise
  = getEmptyCells2
      xss (if j == (length xss - 1) then i + 1 else i)
      (if j == (length xss - 1) then 0 else j + 1)


-- Utility method to show a sudoku
-- show sudoku with
-- >>> putStr (showSudoku sudoku)
showSudoku :: [[Int]] -> String
showSudoku xss = unlines $ intercalate [showDivider] $ chunksOf squareSize $ map showRow xss
  where
    size = length xss
    squareSize = intRoot size 
    numberSize = size `div` 10 + 1

    showRowSection xs = unwords $ map (printf ("%0" ++ show numberSize ++ "d")) xs
    showRow xs = intercalate "|" $ map showRowSection $ chunksOf squareSize xs
    showDivider = intercalate "+" $ replicate squareSize $ replicate ((numberSize + 1) * squareSize - 1) '-'

    chunksOf :: Int -> [e] -> [[e]]
    chunksOf i [] = []
    chunksOf i ls = take i ls : chunksOf i (drop i ls)

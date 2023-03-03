module Exercise10 where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Map as Map (lookup)
import System.IO

import Turtle

-- A Lindenmayer system (L-system) consists of an alphabet, a start pattern, and rewrite rules
-- data type for L-System
data Rule = Char :->: String                
  deriving (Eq,Show)

data LSystem = LSystem {
                  start :: String,
                  rules :: [Rule]           
                  }
  deriving Eq

instance Show LSystem where
  show (LSystem s r) = unlines $ ["Start: " ++ show s, "Rules: "] ++ map show r


apply :: Char -> Turtle -> Turtle
apply 'F' t = move 10 t
apply 'G' t = move 10 t
apply '+' t = turn 30 t
apply '-' t = turn (-30) t
apply '*' t = turn 15 t
apply '~' t = turn (-15) t
apply _ t = sit t

-- Use apply to convert movements of turtle to GL lines.
execute :: LSystem -> Integer -> [Ln]
execute ls n = let (pen, ang, pnt, ln, bps) = lines (black, 0, 0, [],[]) $ expandLSystem ls n in ln
  where
    lines t [] = t
    lines t (x:xs) = lines (apply x t) xs

-- sample LSystems for (manual) testing
dragoncurve :: LSystem
dragoncurve = LSystem "FX" ['X' :->: "X+++YF+++", 'Y' :->: "---FX---Y"]

kochcurve :: LSystem
kochcurve = LSystem "F" ['F' :->: "F+++F---F---F+++F"]

sierpinski :: LSystem
sierpinski = LSystem "F++++G++++G" ['F' :->: "F++++G----F----G++++F", 'G' :->: "GG"]

-- retrieves the rule with the given character on the left-hand side
findRule :: [Rule] -> Char -> Rule
findRule [] c = (c :->: [c])
findRule ((s :->: r):rs) c = if s == c then (s :->: r) else findRule rs c

-- expand a given L-system for a given number of times and return the resulting sequence of symbols
expandLSystem :: LSystem -> Integer -> String
expandLSystem ls 0 = start ls
expandLSystem ls n = expandLSystem (LSystem (concatStringWithRules (rules ls) (start ls)) (rules ls)) (n-1)

concatStringWithRules :: [Rule] -> String -> String
concatStringWithRules _ "" = ""
concatStringWithRules r ss = concat [ getRuleString (findRule r s) | s <- ss]

getRuleString :: Rule -> String
getRuleString (s :->: r) = r

-- receive an L-system, reads lines as long as there is input, and then interpret each line sequentially as a command
update :: LSystem -> IO LSystem
update ls = do
  flag <- (hReady stdin)
  if flag
    then
      do
      xs <- getLine
      let l = getNewLSystem ls xs
      printMsg (getOutStr ls xs)
      update l
    else
      return ls

printMsg :: String -> IO ()
printMsg s
  | (length s) == 0 = return ()
  | otherwise = do putStr s

getNewLSystem :: LSystem -> String -> LSystem
getNewLSystem ls line =
  case (getCommand line) of
    "start" -> LSystem (getVal line) (rules ls)
    "rule" -> LSystem (start ls) (updateRules (rules ls) (getRulesFromStr (getVal line)))
    "clear" -> LSystem "" []
    otherwise -> ls

getOutStr :: LSystem -> String -> String
getOutStr ls line =
  case (getCommand line) of
    "start" -> ""
    "rule" -> if (getRulesFromStr (getVal line)) == Nothing then "Error parsing rule\n" else ""
    "clear" -> ""
    "print" -> ((show ls) ++ "\n")
    otherwise -> "Error parsing command\n"

getCommand :: String -> String
getCommand "" = ""
getCommand (s:ss) = if s == ' ' then "" else s : (getCommand ss)

getVal :: String -> String
getVal "" = ""
getVal (s:ss) = if s == ' ' then ss else getVal ss

getRulesFromStr :: String -> Maybe Rule
getRulesFromStr s
  | (length s) < 6 = Nothing
  | s!!1 == ' ' && s!!2 == '-' && s!!3 == '>' && s!!4 == ' ' = Just (head s :->: tail (tail (tail (tail (tail s)))))
  | otherwise = Nothing

-- getRulesFromStr "" = Nothing
-- getRulesFromStr (s:ss) = (createRule s (getExpression ss))
--
-- getExpression :: String -> Maybe String
-- getExpression "" = Nothing
-- getExpression (s:ss) = if s == '-' && (length ss) > 2
--                         then
--                           if (head ss) == '>' then Just (tail (tail ss)) else Nothing
--                         else
--                           getExpression ss
--
-- createRule :: Char -> Maybe String -> Maybe Rule
-- createRule _ Nothing = Nothing
-- createRule c (Just str) = Just (c :->: str)
--
updateRules :: [Rule] -> Maybe Rule -> [Rule]
updateRules r Nothing = r
updateRules rs (Just (s :->: r)) = (s :->: r) : [ (x :->: y) | (x :->: y) <- rs, x /= s  ]


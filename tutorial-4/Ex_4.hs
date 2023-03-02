module Exercise04 where

-- check the syntax of a string for the simplified XML rules
xmlLight :: String -> Bool
xmlLight "" = True
xmlLight (x:xs)
  | x == '>' = False
  | x == '/' = False
  | x == '<' = openTag xs [] ""
  | otherwise = xmlLight xs

openTag :: String -> [String] -> String -> Bool
openTag "" _ _ = False
openTag (s:xml) tags tag
  | s == '/' = False
  | s == '>' = if length tag == 0 then False else findNextTag xml (tags ++ [tag])
  | s == ' ' = if length tag == 0 then False else findCloseBracketForOpenTag xml tags tag
  | otherwise = openTag xml tags (tag ++ [s])

closeTag :: String -> [String] -> String -> Bool
closeTag "" _ _ = False
closeTag (s:xml) tags tag
  | s == '/' = False
  | s == '>' = if length tags == 0 || length tag == 0 || not (last tags == tag) then False else findNextTag xml (init tags)
  | s == ' ' = findCloseBracketForCloseTag xml tags tag
  | otherwise = closeTag xml tags (tag ++ [s])

findNextTag :: String -> [String] -> Bool
findNextTag "" [] = True
findNextTag "" _ = False
findNextTag (x:xml) tags
  | x == '>' = False
  | x == '<' = processingTag xml tags
  | otherwise = findNextTag xml tags

processingTag :: String -> [String] -> Bool
processingTag "" _ = False
processingTag (x:xml) tags
  | x == '/' = closeTag xml tags ""
  | otherwise = openTag ([x] ++ xml) tags ""

findCloseBracketForOpenTag :: String -> [String] -> String -> Bool
findCloseBracketForOpenTag "" _ _ = False
findCloseBracketForOpenTag (s:xml) tags tag
  | s == ' ' = findCloseBracketForOpenTag xml tags tag
  | s == '>' = openTag (s:xml) tags tag
  | otherwise = False

findCloseBracketForCloseTag :: String -> [String] -> String -> Bool
findCloseBracketForCloseTag "" _ _ = False
findCloseBracketForCloseTag (s:xml) tags tag
  | s == ' ' = findCloseBracketForCloseTag xml tags tag
  | s == '>' = closeTag (s:xml) tags tag
  | otherwise = False

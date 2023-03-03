module Exercise09 where

import Types

import Data.List (sort, sortOn)
import Data.Ord (Down(Down))

-- things from the tutorial

instance Show Polarity where
  show Pos = ""
  show Neg = "~"

instance Show ELiteral where
  show (Literal p a) = show p ++ a

lName :: Literal -> Name
lName (Literal _ n) = n

lPos :: Name -> Literal
lPos = Literal Pos

lNeg :: Name -> Literal
lNeg = Literal Neg

lIsPos :: Literal -> Bool
lIsPos (Literal p _) = p == Pos

lIsNeg :: Literal -> Bool
lIsNeg = not . lIsPos

lNegate :: Literal -> Literal
lNegate (Literal Pos n) = Literal Neg n
lNegate (Literal Neg n) = Literal Pos n

complements :: Literal -> Literal -> Bool
complements (Literal p n) (Literal p' n') = p /= p' && n == n'

evalLiteral :: Valuation -> Literal -> Bool
evalLiteral val l@(Literal _ n)
  | lIsPos l  = n `elem` val
  | otherwise = n `notElem` val

evalClause :: Valuation -> Clause -> Bool
evalClause = any . evalLiteral

eval :: Valuation -> ConjForm -> Bool
eval = all . evalClause

clauseIsTauto :: Clause -> Bool
clauseIsTauto [] = False
clauseIsTauto (l:ls) = lNegate l `elem` ls || clauseIsTauto ls

-- my code starts here

-- take a variable n and clauses cp and cn and resolve cp with cn on the variable n while assuming that n occurs positively in cp and negatively in cn
resolve :: Name -> Clause -> Clause -> Clause
resolve n cp cn = [xp | xp <- cp, lName xp /= n || lIsNeg xp] ++ [xn | xn <- cn, lName xn /= n || lIsPos xn]

-- return a list of all possible resolution steps and resulting clauses for two given clauses
resolvants :: KeyClause -> KeyClause -> [(Resolve, Clause)]
resolvants c1 c2 = [ y | y <- [resolvants_helper (lName c) c1 c2 | c <- snd c1, lIsPos c && lNegate c `elem` snd c2], not (clauseIsTauto (snd y))] ++
                   [ y | y <- [resolvants_helper (lName c) c2 c1 | c <- snd c2, lIsPos c && lNegate c `elem` snd c1], not (clauseIsTauto (snd y))]

resolvants_helper :: Name -> KeyClause -> KeyClause -> (Resolve, Clause)
resolvants_helper n c1 c2 = ((Resolve n (fst c1) (fst c2)), canonicForm (resolve n (snd c1) (snd c2)))

-- proof checking

-- checks a proof against a given formula in CNF
proofCheck :: ConjForm -> Proof -> Bool
proofCheck cf (Model val) = eval val cf
proofCheck cf (Refutation []) = isExistEmpty cf
proofCheck cf (Refutation (p:pr)) = if isExistEmpty cf then True else proofCheck (cf ++ [proofCheckHelper cf p]) (Refutation pr)

proofCheckHelper :: ConjForm -> Resolve -> Clause
proofCheckHelper cf (Resolve n c1 c2) = resolve n (cf!!c1) (cf!!c2)

isExistEmpty :: ConjForm -> Bool
isExistEmpty cf = [] `elem` cf

-- pick the next best clause from U. If U is empty, Nothing must be returned
selClause :: SelClauseStrategy
selClause = selClause_helper

selClause_helper :: KeyConjForm -> Maybe KeyClause
selClause_helper [] = Nothing
selClause_helper cf = selClause_helper_h cf Nothing

selClause_helper_h :: KeyConjForm -> Maybe KeyClause -> Maybe KeyClause
selClause_helper_h [] r = r
selClause_helper_h cf Nothing = if clauseIsTauto (snd (head cf)) then selClause_helper_h (tail cf) Nothing else selClause_helper_h (tail cf) (Just (head cf))
selClause_helper_h cf (Just r) = if not (clauseIsTauto (snd (head cf))) && length (canonicForm (snd (head cf))) < length (canonicForm (snd r))
                                  then selClause_helper_h (tail cf) (Just (head cf))
                                  else selClause_helper_h (tail cf) (Just r)

-- return a proof as well as all processed and unprocessed clauses
resolutionParam :: SelClauseStrategy -> ConjForm -> (Proof, KeyConjForm)
resolutionParam f cf = resolutionParam_helper f (zip [0..(length cf)-1] cf) (length cf) [] []

resolutionParam_helper :: SelClauseStrategy -> KeyConjForm -> Int -> KeyConjForm -> [Resolve] -> (Proof, KeyConjForm)
resolutionParam_helper f u lc p r = resolutionParam_helper_unique f u lc p r (f u)

resolutionParam_helper_unique :: SelClauseStrategy -> KeyConjForm -> Int -> KeyConjForm -> [Resolve] -> Maybe KeyClause -> (Proof, KeyConjForm)
resolutionParam_helper_unique f u lc p r Nothing = resolutionParam_helper_h f u lc p r Nothing
resolutionParam_helper_unique f u lc p r (Just uc) = if (clauseIsInConjForm (snd (unzip p)) (snd uc))
                                                     then resolutionParam_helper f (removeClause u uc) lc p r
                                                     else resolutionParam_helper_h f u lc p r (Just uc)

clauseIsInConjForm :: ConjForm -> Clause -> Bool
clauseIsInConjForm [] _ = False
clauseIsInConjForm (c:cf) cl = if (length (canonicForm c)) == (length (canonicForm cl)) && (all (\x -> x `elem` (canonicForm c)) (canonicForm cl))
                               then True
                               else clauseIsInConjForm cf cl

resolutionParam_helper_h :: SelClauseStrategy -> KeyConjForm -> Int -> KeyConjForm -> [Resolve] -> Maybe KeyClause -> (Proof, KeyConjForm)
resolutionParam_helper_h _ [] _ p _ _ = ((Model (extractModel (snd (unzip p)))), p)
resolutionParam_helper_h _ u _ p r Nothing = ((Model (extractModel (snd (unzip p)))), u ++ p)
resolutionParam_helper_h _ u _ p r (Just (_, [])) = (Refutation r, u ++ p)
resolutionParam_helper_h f u lc p r (Just uc) = addResolvants f (removeClause u uc) lc p r uc (resolvants_helper_h u uc p)

addResolvants :: SelClauseStrategy -> KeyConjForm -> Int -> KeyConjForm -> [Resolve] -> KeyClause -> [(Resolve, Clause)] -> (Proof, KeyConjForm)
addResolvants f u lc p r uc rs =
  resolutionParam_helper f (u ++ zip [lc..lc+(length rs)-1] (snd (unzip rs))) (lc + length rs) (p ++ [uc]) (r ++ [ (fst x) | x <- rs])

removeClause :: KeyConjForm -> KeyClause -> KeyConjForm
removeClause cf c = [ x | x <- cf, x /= c]

resolvants_helper_h :: KeyConjForm -> KeyClause -> KeyConjForm -> [(Resolve, Clause)]
resolvants_helper_h u c cf = [ y | y <- (concat [ resolvants (canonicKeyForm c) (canonicKeyForm x) | x <- cf ]),
                                   not (clauseIsInConjForm (snd (unzip cf)) (snd y)) && not (clauseIsInConjForm (snd (unzip u)) (snd y))]

canonicForm :: Clause -> Clause
canonicForm [] = []
canonicForm (x:xs) = if x `elem` (canonicForm xs) then canonicForm xs else x:(canonicForm xs)

canonicKeyForm :: KeyClause -> KeyClause
canonicKeyForm (k, xs) = (k, canonicForm xs)

-- extract a model as described on https://lara.epfl.ch/w/_media/sav08/gbtalk.pdf

instance Ord Polarity where
  Neg <= Pos = False
  _ <= _ = True

instance Ord ELiteral where
  (Literal p a) <= (Literal p' a') = a < a' || a == a' && p <= p'

extractModel :: ConjForm -> Valuation
extractModel = run [] . sort . map (sortOn Down)
  where
    run val [] = val
    run val (c:cs)
      | evalClause val c = run val cs
      | otherwise = run (lName (head c):val) cs

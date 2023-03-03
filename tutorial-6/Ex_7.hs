module Exercise07 where

-- define a datatype of trees and directions to navigate the tree
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

node x = Node Leaf x Leaf

data Direction = L | R
  deriving (Show, Eq)

-- navigate to the subtree specified by the list of directions and returns it
subtreeAt :: [Direction] -> Tree a -> Maybe (Tree a)
subtreeAt [] a = Just a
subtreeAt _ Leaf = Nothing
subtreeAt (x:xs) (Node left node right)
  | x == L = subtreeAt xs left
  | x == R = subtreeAt xs right

-- performs a right-rotation on the tree
rotateR :: Tree a -> Maybe (Tree a)
rotateR Leaf = Nothing
rotateR (Node left node right)
  | rotateRCheck left = Nothing
  | otherwise = rotateRHelper (Node left node right)

rotateRHelper :: Tree a -> Maybe (Tree a)
rotateRHelper (Node (Node left_left left_node left_right) node right) = Just (Node left_left left_node (Node left_right node right))

rotateRCheck :: Tree a -> Bool
rotateRCheck Leaf = True
rotateRCheck _ = False

-- performs a right-rotation at the location specified by the list of directions
rotateRAt :: [Direction] -> Tree a -> Maybe (Tree a)
rotateRAt [] a = rotateR a
rotateRAt _ Leaf = Nothing
rotateRAt (x:xs) (Node left node right)
  | x == L = rotateRAtHelperLeft (rotateRAt xs left) (Node left node right)
  | x == R = rotateRAtHelperRight (rotateRAt xs right) (Node left node right)

rotateRAtHelperLeft :: Maybe (Tree a) -> Tree a -> Maybe (Tree a)
rotateRAtHelperLeft Nothing _ = Nothing
rotateRAtHelperLeft (Just a) (Node left node right) = Just (Node a node right)

rotateRAtHelperRight :: Maybe (Tree a) -> Tree a -> Maybe (Tree a)
rotateRAtHelperRight Nothing _ = Nothing
rotateRAtHelperRight (Just a) (Node left node right) = Just (Node left node a)

-- returns True if, and only if, the given tree is a right-linear chain
isRchain :: Tree a -> Bool
isRchain Leaf = True
isRchain (Node left node right) = if rotateRCheck left then isRchain right else False

-- every tree can be transformed into a right-linear chain using only right-rotations
rotateRchainDirs :: Tree a -> [[Direction]]
rotateRchainDirs Leaf = []
rotateRchainDirs (Node left node right)
  | isRchain (Node left node right) = []
  | rotateRCheck left == False = [[]] ++ rotateRchainDirs (rotateRchainDirsHelper (rotateR (Node left node right)))
  | otherwise = map rotateRchainDirsMap (rotateRchainDirs right)

rotateRchainDirsHelper :: Maybe (Tree a) -> Tree a
rotateRchainDirsHelper (Just a) = a

rotateRchainDirsMap :: [Direction] -> [Direction]
rotateRchainDirsMap xs = R : xs

-- return a right linear chain
rotateRchain :: [[Direction]] -> Tree a -> Maybe (Tree a)
rotateRchain [] a = Just a
rotateRchain (x:xs) a = rotateRchainHelper xs (rotateRAt x a)

rotateRchainHelper :: [[Direction]] -> Maybe (Tree a) -> Maybe (Tree a)
rotateRchainHelper _ Nothing = Nothing
rotateRchainHelper xs (Just a) = rotateRchain xs a

class Finite a where
  -- finite list of all values of type a
  enumerated :: [a]

-- add instance for `Finite (Maybe a)` here assuming that `Finite a`
instance Finite a => Finite (Maybe a) where
  enumerated = Nothing : (map finiteMaybeHelper enumerated)

finiteMaybeHelper :: a -> Maybe a
finiteMaybeHelper a = Just a

-- Time stores hours, minutes, seconds in this order
data Time = Time Int Int Int deriving (Eq, Show)

-- instance for Finite Time
instance Finite Time where
  enumerated = [ (Time h m s) | h <- [0..23], m <- [0..59], s <- [0..59]]

-- instance for `Finite (Either a b)` here assuming that `Finite a` and `Finite b`
instance (Finite a, Finite b) => Finite (Either a b) where
  enumerated = [ (Left a) | a <- enumerated] ++ [ (Right b) | b <- enumerated]

-- instance for `Finite (a, b)` here assuming that `Finite a` and `Finite b`
instance (Finite a, Finite b) => Finite (a, b) where
  enumerated = [ (a, b) | a <- enumerated, b <- enumerated]

-- instance for `Finite (Tree a)` here assuming that `Finite a`
instance (Eq a, Finite a) => Finite (Tree a) where
  enumerated = finiteTreeHelper enumerated

removeArrayItem :: Eq a => [a] -> a -> [a]
removeArrayItem [] _ = []
removeArrayItem (a:as) b
  | a == b = removeArrayItem as b
  | otherwise = a : removeArrayItem as b

finiteTreeHelper :: Eq a => [a] -> [Tree a]
finiteTreeHelper a
  | length a == 0 = [ Leaf ]
  | length a == 1 = [ (Node Leaf (head a) Leaf) ]
  | otherwise = (concat [ buildTree a b | b <- a ])

buildTree :: Eq a => [a] -> a -> [Tree a]
buildTree a b = [ (Node l b r) | l <- ((finiteTreeHelper (removeArrayItem a b)) ++ [ Leaf ]),
                                 r <- ((finiteTreeHelper (removeArrayItem a b)) ++ [ Leaf ]),
                                 l /= r]


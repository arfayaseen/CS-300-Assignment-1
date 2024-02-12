{-# LANGUAGE FlexibleInstances #-}
import Test.Hspec 

-- Use the following data types for the questions below
data Tree a = Nil | TreeNode (Tree a) a (Tree a) deriving (Show, Eq)

data LinkedList a = Null | ListNode a (LinkedList a) deriving (Show, Eq) 

-- HELPER FUNCTIONS

isSorted2D :: Ord a => [[a]] -> Bool
isSorted2D [] = True
isSorted2D [l] = True
isSorted2D (x:y:ys)
  | null x || null y = isSorted2D (y:ys)
  | otherwise = head x <= head y && isSorted2D (y:ys)

bubble2D :: Ord a => [[a]] -> [[a]]
bubble2D [] = []
bubble2D [x] = [x]
bubble2D (x:y:ys)
  | null x || null y = x : bubble2D (y:ys)
  | head x <= head y = x : bubble2D (y:ys)
  | otherwise = y : bubble2D (x:ys)

bubbleSort2D :: Ord a => [[a]] -> [[a]]
bubbleSort2D [] = []
bubbleSort2D [[x,y]] = [[x,y]]
bubbleSort2D list = if isSorted2D list
                        then list
                        else bubbleSort2D (bubble2D list)

toArray :: LinkedList a -> [a]
toArray Null = []
toArray (ListNode x xs) = x : toArray xs

reverseList :: [a] -> [a]
reverseList [] = []
reverseList [x] = [x]
reverseList (x:xs) = reverseList xs ++ [x]

getChildren :: Tree a -> [Tree a]
getChildren Nil = []
getChildren (TreeNode left _ right) = filterNew isTreeNode [left, right]

isTreeNode :: Tree a -> Bool
isTreeNode Nil = False
isTreeNode _ = True

getValue :: Tree a -> [a]
getValue Nil = []
getValue (TreeNode _ v _) = [v]

filterNew :: (a -> Bool) -> [a] -> [a]
filterNew _ [] = []
filterNew predicate (x:xs) =
    if predicate x
    then x : filterNew predicate xs
    else filterNew predicate xs

concatMapNew :: (a -> [b]) -> [a] -> [b]
concatMapNew _ [] = []
concatMapNew f (x:xs) = f x ++ concatMapNew f xs

mapNew :: (t -> a) -> [t] -> [a]
mapNew f [] = []
mapNew f  (x:xs) = f x : mapNew f xs 

listLength :: Num p => [a] -> p
listLength [] = 0
listLength (x : xs) =  1 + listLength xs

arrayToLinkedList :: String -> LinkedList Char
arrayToLinkedList [] = Null
arrayToLinkedList (x:xs) = ListNode x (arrayToLinkedList xs)

maxNumber :: Ord a => [a] -> a
maxNumber [] = error "empty list"
maxNumber [x] = x
maxNumber (x:y:ys) = if x > y then maxNumber (x:ys) else maxNumber (y:ys)


--Category: Easy

-- Question 1
targetSum :: [Int] -> Int ->[[Int]]
targetSum [] t = []
targetSum list t = bubbleSort2D [[a, b] | a <- list, b <- list, a >= b, a + b == t]

-- -- Question 2
symmetricTree :: Eq a => Tree a -> Bool
symmetricTree Nil = True
symmetricTree (TreeNode left _ right) = isReflection left right

isReflection :: Eq a => Tree a -> Tree a -> Bool
isReflection Nil Nil = True
isReflection Nil (TreeNode {}) = False
isReflection (TreeNode {}) Nil = False
isReflection (TreeNode ll lm lr) (TreeNode rl rm rr) = (lm == rm) && (isReflection ll rr) && (isReflection lr rl)

-- -- Question 3
palindromList :: Eq a => LinkedList a -> Bool
palindromList Null = True
palindromList linkedlist = toArray linkedlist == reverseList (toArray linkedlist)

-- Question 4
snakeTraversal :: Tree a -> [a]
snakeTraversal tree = snakeHelper [tree] True

alternateReverse :: Bool -> [a] -> [a]
alternateReverse True list = list
alternateReverse False list = reverseList list

snakeHelper :: [Tree a] -> Bool -> [a]
snakeHelper [] _ = []
snakeHelper trees direction = alternateReverse direction (concatMapNew getValue trees) ++ snakeHelper (concatMapNew getChildren trees) (not direction)

-- -- Question 5
treeConstruction :: String -> Tree Char
treeConstruction str = treeHelper str Nil

treeHelper :: [Char] -> Tree Char -> Tree Char
treeHelper [] tree = tree
treeHelper ('^':xs) Nil = Nil
treeHelper ('^':"") tree = tree
treeHelper ('^':x:xs) tree = treeHelper xs (insertSibling x tree)
treeHelper (x:xs) Nil = treeHelper xs (TreeNode Nil x Nil)  
treeHelper (x:xs) tree = treeHelper xs (insertLeftofRight x tree)

insertSibling :: Char -> Tree Char -> Tree Char
insertSibling x Nil = TreeNode Nil x Nil
insertSibling x (TreeNode (TreeNode Nil left Nil) val Nil) = TreeNode (TreeNode Nil left Nil) val (TreeNode Nil x Nil)
insertSibling x (TreeNode left val Nil) = TreeNode (insertSibling x left) val Nil
insertSibling x (TreeNode left val right) = TreeNode left val (insertSibling x right)

insertLeftofRight :: Char -> Tree Char -> Tree Char
insertLeftofRight x Nil = TreeNode Nil x Nil
insertLeftofRight x (TreeNode Nil val Nil) = TreeNode (TreeNode Nil x Nil) val Nil
insertLeftofRight x (TreeNode left val Nil) = TreeNode (insertLeftofRight x left) val Nil
insertLeftofRight x (TreeNode left val right) = TreeNode left val (insertLeftofRight x right)

-- Category: Medium

-- Attempy any 4 questions from this category

-- Question 1.1: Overload the (+) operator for Tree. You only need to overload (+). Keep the rest of the operators as undefined.   
instance Num (Tree Int) where
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined
    negate = undefined
    (+) Nil t = t
    (+) t Nil = t 
    (+) (TreeNode Nil x Nil) (TreeNode Nil y Nil) = TreeNode Nil (x + y) Nil
    (+) (TreeNode leftA valA rightA) (TreeNode leftB valB rightB) = TreeNode (leftA + leftB) (valA + valB) (rightA + rightB)

-- -- Question 1.2

longestCommonString :: LinkedList Char -> LinkedList Char -> LinkedList Char
longestCommonString ll1 ll2 = arrayToLinkedList (longestSublist (commonElements (substrings (toArray ll1)) (substrings (toArray ll2))))

substrings :: [Char] -> [String]
substrings [] = [""]
substrings str = [take n (drop m str) | m <- [0..(length str)], n <- [1..(length str - m)]]

commonElements :: Eq a => [a] -> [a] -> [a]
commonElements [] list = []
commonElements list [] = []
commonElements list1 list2 = [x | x <- list1, elemNew x list2]

elemNew :: Eq a => a -> [a] -> Bool
elemNew _ [] = False
elemNew x (y:ys)
  | x == y = True
  | otherwise = elemNew x ys

findIndex :: (Eq t, Num p) => t -> [t] -> p
findIndex x [] = error "not found"
findIndex x list = finder x list 0

finder :: (Eq t1, Num t2) => t1 -> [t1] -> t2 -> t2
finder x [] _ = error "not found"
finder x (y:ys) num = if x == y then num else finder x ys num+1

longestSublist :: [[a]] -> [a]
longestSublist [] = []
longestSublist list2D = list2D !! (findIndex (maxNumber (countLengths list2D)) (countLengths list2D))

countLengths :: Num a1 => [[a2]] -> [a1]
countLengths [] = []
countLengths list = mapNew listLength list

-- -- Question 2
commonAncestor :: Ord a => Eq a => Tree a -> a -> a -> Maybe a
commonAncestor Nil _ _ = Nothing
commonAncestor tree x y = if (isPresent x tree) && (isPresent y tree) then commonAncestorHelper tree x y else Nothing

commonAncestorHelper :: (Ord a) => Tree a -> a -> a -> Maybe a
commonAncestorHelper Nil _ _ = Nothing
commonAncestorHelper (TreeNode left val right) p q
    | val < p && val < q = commonAncestorHelper right p q 
    | val > p && val > q = commonAncestorHelper left p q
    | otherwise = Just val

isPresent :: (Ord a) => a -> Tree a -> Bool
isPresent _ Nil = False
isPresent target (TreeNode left val right)
    | target == val = True
    | target < val = isPresent target left
    | otherwise = isPresent target right

-- Question 3

-- gameofLife :: [[Int]] -> [[Int]]
-- gameofLife = undefined

-- -- Question 4
-- waterCollection :: [Int] -> Int
-- waterCollection = undefined

-- -- Question 5
-- minPathMaze :: [[Int]] -> Int
-- minPathMaze = undefined





-- Main Function
main :: IO ()
main =
   hspec $ do

    -- Test List Target Sum
        describe "targetSum" $ do
            it "should return pairs whose sum is equal to the target" $ do
                targetSum [1,2,3,4,5] 5 `shouldBe` [[3,2], [4,1]]
                targetSum [1,2,3,4,5,6] 10 `shouldBe` [[5,5], [6,4]]
                targetSum [1,2,3,4,5] 0 `shouldBe` []
                targetSum [1,10,8,7,6,2,3,4,5,-1,9] 10 `shouldBe` [[5,5],[6,4],[7,3],[8,2],[9,1]]
    
    -- -- Test Symmetric Tree
        describe "symmetricTree" $ do
            it "should return True if the tree is symmetric" $ do
                symmetricTree (Nil :: Tree Int) `shouldBe` True
                symmetricTree (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 1 Nil)) `shouldBe` True
                symmetricTree (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 2 Nil)) `shouldBe` False
                symmetricTree (TreeNode (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 4 (TreeNode (TreeNode Nil 3 Nil) 2 (TreeNode Nil 1 Nil))) `shouldBe` True
                symmetricTree (TreeNode (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 4 (TreeNode (TreeNode Nil 3 Nil) 2 (TreeNode Nil 4 Nil))) `shouldBe` False
    
    -- -- Test Palindrom List
        describe "palindromList" $ do
            it "should return True if the list is a palindrome" $ do
                palindromList (Null :: LinkedList Int) `shouldBe` True
                palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 2 (ListNode 1 Null))))) `shouldBe` True
                palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 3 (ListNode 1 Null))))) `shouldBe` False
                palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 2 (ListNode 2 Null))))) `shouldBe` False
                palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 2 (ListNode 1 (ListNode 1 Null)))))) `shouldBe` False
                palindromList (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'b' (ListNode 'a' Null))))) `shouldBe` True
                palindromList (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'c' (ListNode 'a' Null))))) `shouldBe` False
    
    -- Test Snake Traversal
        describe "snakeTraversal" $ do
            it "should return the snake traversal of the tree" $ do
                snakeTraversal (Nil:: Tree Int) `shouldBe` []
                snakeTraversal (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) `shouldBe` [2,3,1]
                snakeTraversal (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode Nil 7 Nil))) `shouldBe` [4,2,3,1,6,5,7]
                snakeTraversal (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode (TreeNode Nil 9 Nil) 7 Nil))) `shouldBe` [4,2,3,1,6,5,7,9]
    
    -- Test Tree Construction
        describe "treeConstruction" $ do
            it "should return the tree constructed from the string" $ do
                treeConstruction "" `shouldBe` Nil
                treeConstruction "a" `shouldBe` TreeNode Nil 'a' Nil
                treeConstruction "^a" `shouldBe` Nil
                treeConstruction "ab^c" `shouldBe` TreeNode (TreeNode Nil 'b' Nil) 'a' (TreeNode Nil 'c' Nil)
                treeConstruction "ab^c^" `shouldBe` TreeNode (TreeNode Nil 'b' Nil) 'a' (TreeNode Nil 'c' Nil)
                treeConstruction "ab^cde^f" `shouldBe` TreeNode (TreeNode Nil 'b' Nil) 'a' (TreeNode (TreeNode (TreeNode Nil 'e' Nil) 'd' (TreeNode Nil 'f' Nil)) 'c' Nil)
                treeConstruction "abcde^f" `shouldBe` TreeNode (TreeNode (TreeNode (TreeNode (TreeNode Nil 'e' Nil) 'd' (TreeNode Nil 'f' Nil)) 'c' Nil) 'b' Nil) 'a' Nil
    
    -- Test (+) operator for Tree
        describe "(+)" $ do
            it "should return the sum of the two trees" $ do
                let result1 = (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil) + TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil) :: Tree Int) 
                result1  `shouldBe` TreeNode (TreeNode Nil 2 Nil) 4 (TreeNode Nil 6 Nil) 
                let result2 = (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil) + TreeNode Nil 2 (TreeNode Nil 3 Nil) :: Tree Int)
                result2 `shouldBe` TreeNode (TreeNode Nil 1 Nil) 4 (TreeNode Nil 6 Nil)
                let result3 = (Nil + Nil :: Tree Int) 
                result3 `shouldBe` Nil
                let result4 = (Nil + TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil):: Tree Int)
                result4 `shouldBe` TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)
                let result5 = (TreeNode (TreeNode (TreeNode Nil 1 (TreeNode Nil (-2) Nil)) 3 Nil) 4 (TreeNode Nil 2 (TreeNode Nil 7 (TreeNode Nil (-7) Nil))) + TreeNode (TreeNode (TreeNode (TreeNode Nil 0 Nil) 1 Nil) 3 (TreeNode (TreeNode Nil 1 Nil) 6 (TreeNode Nil (-2) Nil))) 4 (TreeNode (TreeNode (TreeNode Nil 9 Nil) 5 (TreeNode Nil 4 Nil)) 2 (TreeNode (TreeNode Nil (-5) Nil) 7 Nil)) :: Tree Int) 
                result5 `shouldBe` TreeNode (TreeNode (TreeNode (TreeNode Nil 0 Nil) 2 (TreeNode Nil (-2) Nil)) 6 (TreeNode (TreeNode Nil 1 Nil) 6 (TreeNode Nil (-2) Nil))) 8 (TreeNode (TreeNode (TreeNode Nil 9 Nil) 5 (TreeNode Nil 4 Nil)) 4 (TreeNode (TreeNode Nil (-5) Nil) 14 (TreeNode Nil (-7) Nil)))
                let result6 = (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode Nil 7 Nil)) + TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode Nil 7 Nil)) :: Tree Int) 
                result6 `shouldBe` TreeNode (TreeNode (TreeNode Nil 2 Nil) 6 (TreeNode Nil 12 Nil)) 8 (TreeNode (TreeNode Nil 10 Nil) 4 (TreeNode Nil 14 Nil))
    
    -- Test Longest Common String
        describe "longestCommonString" $ do
            it "should return the longest common string" $ do
                longestCommonString Null Null `shouldBe` Null
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) Null `shouldBe` Null
                longestCommonString Null (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) `shouldBe` Null
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) `shouldBe` ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'f' Null))))) `shouldBe` ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' Null)))
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'f' (ListNode 'e' Null))))) `shouldBe` ListNode 'a' (ListNode 'b' (ListNode 'c' Null))
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'b' (ListNode 'f' (ListNode 'g' (ListNode 'e' Null))))) `shouldBe` ListNode 'a' (ListNode 'b' Null)
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'f' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) `shouldBe` ListNode 'c' (ListNode 'd' (ListNode 'e' Null))
    
    -- Test Common Ancestor
        describe "commonAncestor" $ do
            it "should return the lowest common ancestor of the two nodes" $ do
                commonAncestor Nil 1 2 `shouldBe` Nothing
                commonAncestor (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 1 3 `shouldBe` Just 2
                commonAncestor (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 1 4 `shouldBe` Nothing
                commonAncestor (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 4 Nil)) 5 (TreeNode (TreeNode Nil 6 Nil) 8 (TreeNode Nil 9 Nil))) 1 6 `shouldBe` Just 5
                commonAncestor (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 4 Nil)) 5 (TreeNode (TreeNode Nil 6 Nil) 8 (TreeNode Nil 9 Nil))) 8 9 `shouldBe` Just 8
                commonAncestor (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 4 Nil)) 5 (TreeNode (TreeNode Nil 6 Nil) 8 (TreeNode Nil 9 Nil))) 1 3 `shouldBe` Just 3    

    -- Test Game of Life
        -- describe "gameofLife" $ do
        --     it "should return the next state" $ do
        --         gameofLife [[0,1,0],[0,0,1],[1,1,1],[0,0,0]] `shouldBe` [[0,0,0],[1,0,1],[0,1,1],[0,1,0]]
        --         gameofLife [[1,1],[1,0]] `shouldBe` [[1,1],[1,1]]
        --         gameofLife [[1,1],[1,1]] `shouldBe` [[1,1],[1,1]]
        --         gameofLife [[1,0],[0,1]] `shouldBe` [[0,0],[0,0]]
        --         gameofLife [[0,1,0,0],[0,1,1,1],[1,0,1,1]] `shouldBe` [[0,1,0,0],[1,0,0, 1],[0,0,0,1]]
    
    -- -- Test Water Collection
    --     describe "waterCollection" $ do
    --         it "should return the amount of water that can be trapped" $ do
    --             waterCollection [0,1,0,2,1,0,1,3,2,1,2,1] `shouldBe` 12
    --             waterCollection [4,2,0,3,2,5] `shouldBe` 18
    --             waterCollection [1,2,3,4,5] `shouldBe` 0
    --             waterCollection [5,4,3,2,1] `shouldBe` 0
    --             waterCollection [5,4,3,2,1,2,3,4,5] `shouldBe` 32  
    
    -- -- Test Min Path Maze
    --     describe "minPathMaze" $ do
    --         it "should return the minimum cost to reach the bottom right cell" $ do
    --             minPathMaze [[1,3,1],[1,5,1],[4,2,1]] `shouldBe` 7
    --             minPathMaze [[1,2,3],[4,5,6]] `shouldBe` 12
    --             minPathMaze [[1,2,3],[4,5,6],[7,8,9]] `shouldBe` 21
    --             minPathMaze [[1,2,3,4],[4,5,6,7],[7,8,9,9],[10,11,1,13]] `shouldBe` 35
    --             minPathMaze [[1,2,3,4,5],[4,5,6,7,8],[7,8,9,9,10],[10,11,1,13,14],[15,16,17,18,19]] `shouldBe` 66
    --             minPathMaze [[1,2,3,4,5,6],[4,1,2,7,8,9],[7,8,1,2,10,11],[10,11,1,2,22,15],[15,16,17,1,2,20],[21,22,23,24,2,26]] `shouldBe` 41
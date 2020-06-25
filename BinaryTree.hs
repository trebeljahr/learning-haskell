module BinaryTree  
( Tree (..)
, treeAdd
, treeLookup
) where  

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq, Ord)

treeAdd :: (Ord a) => a -> Tree a -> Tree a 
treeAdd elem EmptyTree = genNode elem 
treeAdd elem (Node a left right)
    | elem == a = Node elem left right
    | elem < a = Node a (treeAdd elem left) right
    | elem > a = Node a left (treeAdd elem right)

genNode :: a -> Tree a 
genNode elem = Node elem EmptyTree EmptyTree 

treeLookup :: (Ord a) => a -> Tree a -> Bool 
treeLookup elem EmptyTree = False 
treeLookup elem (Node a left right)
    | elem == a = True 
    | elem < a = treeLookup elem left 
    | elem > a = treeLookup elem right 
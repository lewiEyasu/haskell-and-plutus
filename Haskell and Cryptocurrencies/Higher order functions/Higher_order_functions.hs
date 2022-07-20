{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}
module HigherOrder where

import Data.List (foldl')
import Prelude hiding (take, product, reverse)

-- These are binary trees with labels in their nodes.

data BinTree a =
    Bin (BinTree a) a (BinTree a)
  | Empty
  deriving (Eq, Show)

-- Task HigherOrder-1.
--
-- Define 'product' both using an accumulator explicitly,
-- and using (strict) foldl'.

-- |
-- >>> product [1 .. 4]
-- 24
--
product :: Num a => [a] -> a
product = foldr1 (\x accu -> x * accu)  

-- |
-- >>> product [3, 11]
-- 33
--
product' :: Num a => [a] -> a
product' = foldl (\accu x -> x * accu) 1

-- Task HigherOrder-2.
--
-- Define 'reverse' using 'foldl'.

reverse :: [a] -> [a]
reverse = foldl (\accu x -> x:accu) []

-- Task HigherOrder-3.
--
-- Define a Functor instance for binary trees. For
-- this, we have to define a map function on binary
-- trees and then define the class instance.
--
-- The instance is actually given below. You just
-- have to uncomment it.

-- |
-- >>> mapBinTree (+1) (Bin Empty 7 (Bin Empty 8 Empty))
-- Bin Empty 8 (Bin Empty 9 Empty)
--
mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree p (Bin c x d) =  Bin (mapBinTree p c) (p x) (mapBinTree p d)
mapBinTree _ Empty = Empty  


instance Functor BinTree where
  fmap = mapBinTree


-- Task HigherOrder-4.
--
-- The 'BinTree' type is suitable for representing
-- "binary search trees".
--
-- Binary search trees are trees that store their elements
-- in order, so that we can efficiently find elements by comparing
-- the element we are looking for with the current node, and
-- descending either left or right.
--
-- Define a function 'isBST' that checks if a given 'BinTree'
-- is a binary search tree.

isBST :: Ord a => BinTree a -> Bool
isBST (Bin c@(Bin _ x1 _) x d@(Bin _ x2 _)) = x > x1 && x < x2 && isBST c && isBST d
isBST (Bin Empty x Empty) = True


-- Task HigherOrder-5.
--
-- Define a function 'search' that looks up a value in a BST.
--
-- From now on, we use a type synonym to signal that a certain
-- binary tree should in fact be a binary search tree, even if
-- the type system does not actively enforce this.

type BST a = BinTree a

search :: Ord a => a -> BST a -> Bool
search n x@(Bin c y d) 
        | isBST x = check'
        | otherwise = False
      where check' 
              | n == y = True
              | n > y =  search n d
              | n < y =  search n c

search _ Empty = False              
-- Task HigherOrder-6.
--
-- Define a function 'insert' that inserts a value into a BST
-- while maintaining the BST property. (Don't worry about balancing
-- the tree. That's not important for now. But do make sure you
-- maintain the BST property itself.)

insert :: Ord a => a -> BST a -> BST a
insert n b@(Bin l x r)
         | n < x = Bin (insert n l) x r 
         | n > x = Bin l x (insert n r) 
         | n == x = b
insert n Empty = Bin Empty n Empty 

-- Task HigherOrder-7.
--
-- Define the function 'all' (as in the Prelude) using 'foldr'.
-- Hide the original binding from the Prelude by exluding it in
-- the module header. Provide the type signature yourself.

-- |
all' :: Foldable t => (a->Bool) -> t a -> Bool
all' p = foldr (\x accu -> p x && accu)True
-- >>> all even [2, 4 .. 20]
-- True
--
-- >>> all odd [1, 1, 1, 2, 3, 3]
-- FalseS
--
-- TODO: define all

-- Task HigherOrder-8.
--
-- Import the function 'sortBy' from the 'Data.List' module.
-- Then use this function to define a function that sorts a
-- list in descending rather than ascending order.

sortDescending :: Ord a => [a] -> [a]
sortDescending = error "TODO: define sortDescending"

-- Task HigherOrder-9.
--
-- Use 'insert' and 'foldr' to create a BST from a list.

fromListBST :: Ord a => [a] -> BST a
fromListBST = error "TODO: define fromListBST"

-- Task HigherOrder-10.
--
-- We want to attach unique numbers to each node in a binary
-- tree, so that all the numbers from left to right are labelled
-- in ascending order.
--
-- NOTE: This is not easy. Think about this and discuss your
-- strategy with us before you proceed.

-- |
-- >>> labelTree $ Bin Empty 'x' (Bin Empty 'y' Empty)
-- Bin Empty ('x',1) (Bin Empty ('y',2) Empty)
--
-- >>> labelTree $ Bin (Bin Empty 1 Empty) 2 (Bin Empty 5 Empty)
-- Bin (Bin Empty (1,1) Empty) (2,2) (Bin Empty (5,3) Empty)
--
labelTree :: BinTree a -> BinTree (a, Int)
labelTree = error "TODO: define labelTree"

-- Task HigherOrder-11.
--
-- Another form of tree labeling does not use an integer, but
-- a label supply that is given as a list. So write a variant
-- of 'labelTree' that takes the labels from a list, but uses
-- every label only once. You may assume in this function that
-- the list contains infinitely many (or at least sufficiently
-- many) labels, so you don't have to return a 'Maybe' if the
-- list is too short, but can just crash.

-- |
-- >>> labelTree' (Bin Empty 1 (Bin Empty 42 Empty)) "Haskell"
-- Bin Empty (1,'H') (Bin Empty (42,'a') Empty)
--
labelTree' :: BinTree a -> [b] -> BinTree (a, b)
labelTree' = error "TODO: define labelTree'"

-- Task HigherOrder-12.
--
-- Define the catamorphism on 'BinTree'.
-- Also come up with the type signature yourself.
-- Look at functions such as 'mapBinTree' and 'search'
-- above for inspiration. Also try to
-- rewrite these in terms of the catamorphism once you
-- are done.

-- Task HigherOrder-13.
--
-- Try to implement the function 'take' on lists using 'foldr'.
--
-- Once again, this is not easy, and you should discuss your
-- ideas with us before trying.
--
-- Consider the type signature and a possible definition of
-- take, and note that not just the list is being traversed,
-- but also the number changes.

-- |
-- >>> take 3 "Haskell"
-- "Has"
--
take :: Int -> [a] -> [a]
take _ []       = []
take n (x : xs)
  | n > 0     = x : take (n - 1) xs
  | otherwise = []

-- Task HigherOrder-14.
--
-- If you succeeded in defining 'take' in terms of 'foldr',
-- then perhaps it will not surprise you all that much that
-- even 'foldl' can be written in terms of 'foldr'.
--
-- Try to do this. The approach required is similar.

-- |
-- >>> myFoldl (\xs x -> xs ++ [x]) [] "Haskell"
-- "Haskell"
--
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl = error "TODO: implement myFoldl"

-- Task HigherOrder-15.
--
-- Define a Foldable instance for binary trees.
-- For this, there are several methods, but the
-- easiest with our knowledge so far is to implement
-- a foldr function on trees.
-- For this, one option might be to first convert a
-- binary tree to a list.

foldrBinTree :: (a -> b -> b) -> b -> BinTree a -> b
foldrBinTree = error "TODO: implement foldrBinTree"

{-
instance Foldable BinTree where
  foldr = foldrBinTree
-}
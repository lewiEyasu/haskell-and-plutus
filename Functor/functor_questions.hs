{-# LANGUAGE InstanceSigs #-}
import Data.Bitraversable (Bitraversable)
import Data.Char
{- 1. Implement Functor for binary search trees.-}
data BinarySearchTree a
  = Branch (BinarySearchTree a) a (BinarySearchTree a)
  | Leaf
  deriving(Show, Eq)
instance Functor BinarySearchTree where  
    fmap :: (a -> b) -> BinarySearchTree a -> BinarySearchTree b
    fmap f (Branch l x r) = Branch ( fmap f l) (f x) ( fmap f r) 
    fmap f Leaf = Leaf

{- 2. Implement (<$) for Maybe a.-}
(<$) :: a -> Maybe b -> Maybe a
(<$) a (Just b) = Just a
(<$) _ Nothing = Nothing

{- 3. Implement a function that converts strings to upper case (toUpperString :: String -> String) without using toUpper.-}
toUpperString :: String -> String

toUpperString = fmap (\x -> chr (65 + (ord x - 97))) 
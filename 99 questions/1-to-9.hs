-- Find the last element of a list.

mylast x =  x !! (length x - 1) 


--Find the last but one element of a list.
myButLast :: [a] -> a
myButLast d = case reverse d of (_:y:_) -> y

-- Find the K'th element of a list. The first element in the list is number 1

elementAt :: [a] -> Int -> a
elementAt x n = x !! (n-1)

--Find the number of elements of a list.
myLength :: [a] -> Int
myLength (x:xs) = 1 + myLength xs
myLength _ = 0  

-- Reverse a list.
myReverse :: [a] -> [a]
myReverse (x:xs) = myReverse xs ++ [x]
myReverse  _ = []

-- Find out whether a list is a palindrome. A palindrome can be read forward or backward
isPalindrome ::Eq a => [a] -> Bool
isPalindrome x 
     |x == reverse x  =  True 
     |otherwise = False

-- Flatten a nested list structure.
data NestedList a = Elem a 
     | List [NestedList a]
     deriving (Show) 
--flatten :: NestedList a -> [a] 

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List(x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

--Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress (x:ys@(y: _)) 
       | x == y = compress (ys)
       | x /= y = x: compress (ys)

compress ys = ys

-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack (x:xs) = ([[ a | a <- x:xs, a == x]]) ++ pack [ b | b <- xs, b /= x] 
pack _ = []

-- Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

--encode (x:xs) = (n,letter) :  encode ds
  --   where n = length ([a | a <- x:xs, ])
    --       letter = x
--take 2 $ drop 1 $ "abcd"

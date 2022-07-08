import Data.List (genericLength, sort, insert)
import Distribution.TestSuite (Result(Error))

-- Write a Haskell functionxorthat takes two Booleans and returns the “exclusive-or”
-- of the two values.  An exclusive-or operation returnsTruewhen exactly one
--of its arguments isTrueand returnsFalseotherwise
xor' :: Eq a => a -> a -> Bool
xor' a b 
      | a == b = False
      | otherwise = True 

--Write  a  Haskell  functionmult that  takes  two  natural  numbers  and  returns
--their product.  The function must not use the multiplication (*) or division (/)operators.      
mult :: (Eq t, Num t, Num p) => p -> t -> p
mult a b 
   | b /= 0 = a + mult a (b-1)
   | otherwise = 0

-- Write a Haskell function to compute the maximum value in a nonempty list ofintegers.  Generalize the function by making it polymorphic, accepting a valuefrom any ordered type.   
max' :: Ord a => [a] -> a
max' [] = error "empty list"
max' [x]= x
max' (x:xs) 
    | x > maxnum = x
    | otherwise = maxnum
    where maxnum = max' xs

 --Write  a  Haskell  functionadjpairsthat  takes  a  list  and  returns  the  list  ofall  pairs  of  adjacent  elements.   For  example,adjpairs [2,1,11,4]returns[(2,1), (1,11), (11,4)]
adjpairs :: [b] -> [(b, b)]
adjpairs (x:y:xs) = (x,y) : adjpairs (y:xs)
adjpairs _ = []

--Write a Haskell functionmeanthat takes a list of integers and returns the mean(i.e., average) value for the list.
--mean' :: [Int] -> Float
--mean' :: [Int] -> Float
mean' :: Fractional p => [p] -> p
mean' [] = error "Empty list"
mean' x  = sum x / genericLength x
 
--Hailstone functions
--(a)
hailstone' :: Integer -> Integer
hailstone' 0 = 0 
hailstone' n 
      | n == 1 = 1 
      | even n =  hailstone'  (n `div` 2)
      | odd n = hailstone' ((3 * n) + 1 )

--(b)

hailstoneList :: [Integer] -> [Integer] 
hailstoneList n = [ hailstone' a | a <-n]     

--(c)

hailstone :: Integer -> [Integer]
hailstone 0 = [0] 
hailstone n 
      | n == 1 = [1] 
      | even n = n: hailstone  (n `div` 2)
      | odd n = n : hailstone ((3 * n) + 1 )

-- Number base conversion.

--(a)

natToBin :: Integral a => a -> [a]
natToBin 1 = [1]
natToBin 0 = [0]
natToBin n =  natToBin (n `div` 2) ++ [n `rem` 2]

--(b)

natToBase :: Integral a => a -> a -> [a]
natToBase b n 
      | b < 2 = []
      | n == 0 = [0]
      | n == 1 = [1]
      | otherwise = natToBase b (n `div` b) ++  [n `rem` b]

--(c)

baseToNat :: Integral a => a -> [a] -> a     
baseToNat b (x:y:xs)  = baseToNat' (x * b + y) b xs   

baseToNat' :: Num p => p -> p -> [p] -> p
baseToNat' inti a (d:ds) = baseToNat' (inti * a + d ) a ds
baseToNat' init _ [] = init  

-- Write a Haskell function merge that takes two increasing lists of integers and
--merges them into a single increasing list (without any duplicate values).  A list is
--increasing if every element is less than (<) its successors.  Successor means an
--element that occurs later in the list, i.e., away from the head.  Generalize the
--function by making it polymorphic.

merge' :: (Eq a ,Ord a) => [a] -> [a] -> [a]
merge' (x:xs) [] = x:xs
merge' [] (x:xs)  = x:xs
merge' [] [] = [] 
merge' (d:ds) (x:xs) 
      | x < d = [x] ++ merge' (d:ds) xs  
      | x > d = d : merge' ds (x:xs)
      | x == d = merge' (d:ds) xs



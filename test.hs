zipWith' :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' p (x:xs) (d:ds) = p x d : zipWith' p xs ds 

filp' :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
filp' f = \ x y -> f y x

--filp'' 

map' :: (t -> a) -> [t] -> [a]
map' f (x:xs) = f x : map' f xs  
map' _ [] = []

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) = if p x then x:filter' p xs else filter' p xs 

quicksort' :: (Ord a) =>[a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = smallList ++ [x] ++ largeList
        where smallList = quicksort'(filter' (>= x) xs) 
              largeList = quicksort'(filter' (< x) xs) 

largestDivisible :: (Integral a) => a  
largestDivisible = head (filter' (\xs -> xs `mod`3829 == 0) [100000,99999..])
--          where p x= x `mod` 3829 == 0 

chain :: Integral a => a -> [a]
chain 1 = [1]
chain n 
       | even n = n:chain(n `div` 2)
       | odd n = n: chain (n*3+1)         
elem'' :: (Eq a) => a -> [a] -> Bool  
elem'' y  = foldl (\acc x -> if x==y then True else acc) False 


max'' :: Ord a => [a] -> a
max'' = foldl1 (\accu x -> if accu > x then accu else x)

reverse'' :: [a] -> [a]
reverse'' = foldl (\accu x -> x : accu) []

product'' ::Num (a) => [a]-> a
product'' = foldl1 (\accu x -> x * accu)

head'' :: [a] -> a
head'' = foldr1 (\x _ -> x)

last'' :: [a] -> a
last'' = foldl1 (\_ x -> x)

testList :: Eq a => [a] -> [[a]]
testList [] = []
testList c@(x:xs) = list : testList (drop listLength c)
          where list = takeWhile (== x ) c 
                listLength = length list
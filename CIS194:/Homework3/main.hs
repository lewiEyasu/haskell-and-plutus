{-
Exercise1 Hopscotch 
Your first task is to write a function
The output ofskipsis a list of lists. The first list in the output should
be the same as the input list. The second list in the output should
contain every second element from the input list. . . and thenth list in
the output should contain everynth element from the input list.    
-}

skips :: [a] -> [[a]]
skips a = skips' 1 a

skips' :: Int -> [a] -> [[a]]
skips' d list = if d > (length list) then
                     []
                else     
                     test' list (length_list d list) : skips'(d+1) list

test' :: [b] -> [Int] -> [b]
test' y = map (\x -> y !! (x-1))

length_list num list = filter (\x -> (x `mod` num)  == 0) [1..length list]


{-Exercise2Local maximaAlocal maximum
of a list is an element of the list which is strictly greater than both the elements immediately before and after it.
Forexample, in the list[2,3,4,1,5], the only local maximum is4, sinceit is greater than the elements immediately
before and after it (3and1).5is not a local maximum since there is no element that comesafter it.-}

localMaxima :: [Integer] -> [Integer]
localMaxima (x:xs@(y:d:ds)) = if (y > x && y > d) then y : localMaxima xs else localMaxima xs
localMaxima (_:_:[]) = []   
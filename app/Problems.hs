-- | problem 1. last number in a list
myLast :: [a] -> a
myLast = head . reverse

-- | problem 2. second last element in list
myButLast :: [a] -> a
myButLast = head . tail . reverse

-- | problem 3. element at index, 1 based
elementAt :: Int -> [b] -> b
elementAt x y = head $ flip drop y (x-1)

-- | problem 4. # of elements in list
myLength :: [a] -> Int
myLength = foldr (\_ x -> x+1) 0

myLength' :: [a] -> Int
myLength' xs = snd $ last $ zip xs [1..]

-- | problem 5. reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

-- | problem 6. is same forwards as backwards?
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs =  xs == myReverse xs

-- | problem 7. flatten nested lists
data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List xs) = foldr1 (++) $ map myFlatten xs

-- | problem 8. remove duplicate consecutive entries
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = 
  if x == head xs 
    then compress xs
    else x : compress xs

-- | problem 9. pack
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

-- | problem 10. run length encode
rle :: Eq a => [a] -> [(Int,a)]
rle xs = [(length x, head x) | x <- pack xs]

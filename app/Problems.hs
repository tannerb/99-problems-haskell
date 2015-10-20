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

-- | problem 11. modified run length encode
data RLPair a = Multiple Int a | Single a deriving (Show)

mrle :: Eq a => [a] -> [RLPair a]
mrle xs = [toRLPair x | x <- rle xs]
  where toRLPair (1,y) = Single y
        toRLPair (x,y) = Multiple x y

-- | problem 12. decode mrle
decodeMrle :: Eq a => [RLPair a] -> [a]
decodeMrle [] = []
decodeMrle (x:xs) = decodeRLPair x ++ decodeMrle xs
  where decodeRLPair (Single x) = [x]
        decodeRLPair (Multiple n x) = replicate n x

-- | problem 13. 

-- encodeDirect :: Eq a => [a] -> [RLPair a]
-- encodeDirect (x:xs)

-- | problem 14. Duplicate elements of a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- | problem 15. replicate the elements of list n times
repli :: Int -> [a] -> [a]
repli n [] = []
repli n xs = concatMap (replicate n) xs

-- | problem 16. drop every n'th element
dropEvery :: Int -> [a] -> [a]
dropEvery n [] = []
dropEvery n xs 
    | length xs < n = xs
    | otherwise = (take (n-1) xs) ++ (dropEvery n $ drop n xs)

-- | problem 17. split a list in two given the size of the first
split :: [a] -> Int -> [[a]]
split [] n = []
split xs n = [(take n xs),(drop n xs)]

-- | problem 18. slice a list with i and k
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs i k =  drop (i-1) $ take k xs

-- | problem 19. rotate a list n places to the left
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate [xs] _ = [xs]
rotate xs 0 = xs
rotate (x:y:xs) n = rotate (y:xs ++ [x]) (n-1)
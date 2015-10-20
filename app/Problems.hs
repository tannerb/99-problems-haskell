problemOne :: [a] -> a
problemOne = head . reverse

problemTwo :: [a] -> a
problemTwo = head . tail . reverse

problemThree :: Int -> [b] -> b
problemThree x y = head $ flip drop y (x-1)

problemFour :: [a] -> Int
problemFour = foldr (\_ x -> x+1) 0

problemFive :: [a] -> [a]
problemFive [] = []
problemFive (x:xs) = problemFive xs ++ [x]
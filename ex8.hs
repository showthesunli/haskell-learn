myLen :: [a] -> Int
myLen = foldr (\_ c -> c + 1) 0

myRe :: [a] -> [a]
myRe = foldr (\x xs -> xs ++ [x]) []

ys :: [Int]
ys = [1, 3, 4]

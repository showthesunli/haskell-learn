fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop _ [] = []
myDrop n (x : xs) = myDrop (n - 1) xs

(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x : xs) +++ ys = x : (xs +++ ys)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = smaller ++ [x] ++ greater
    where
        smaller = [s| s<-xs, s<=x]
        greater = [g| g<-xs, g>x]

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (b:bs) = b && myAnd bs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs 

myReplicate :: Int -> a -> [a]
myReplicate 0 x = []
myReplicate n x = [x] ++ myReplicate (n - 1) x

(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(_:xs) !!! n = xs !!! (n-1)

myElem :: Eq a => a -> [a] -> Bool
myElem x [y] = x == y
myElem x (y:ys) = (x == y) || myElem x ys

myMerge :: Ord a => [a] -> [a] -> [a]
myMerge xs [] = xs
myMerge [] ys = ys
myMerge (x:xs) (y:ys) = if x <= y 
    then x : myMerge xs (y:ys)
    else y : myMerge (x:xs) ys
                            
msort :: Ord a => [a] -> [a]
msort [x] = [x]
msort (x:xs) = myMerge [x] (msort xs)
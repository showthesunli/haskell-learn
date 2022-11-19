myLen :: [a] -> Int
myLen = foldr (\_ c -> c + 1) 0

myRe :: [a] -> [a]
myRe = foldr (\x xs -> xs ++ [x]) []

ys :: [Int]
ys = [1, 3, 4]

mf :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mf f p xs = map f (filter p xs)

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\x xs -> (f x) : xs) [] xs

myD :: Int -> [a] -> [a]
myD n (x : xs)
  | n == 0 = x : xs
  | n > 0 = myD (n - 1) xs

reLa :: [a] -> [a]
reLa (x : xs)
  | null xs = []
  | otherwise = x : reLa xs

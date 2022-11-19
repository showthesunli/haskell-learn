factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [1 .. n], prime x]

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (i, x') <- zip [0 ..] xs, x == x']

myCount :: Eq a => a -> [a] -> Int
myCount x xs = length [x | x' <- xs, x == x']

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(n', n'', n) | n' <- [1 .. n - 1], n'' <- [2 .. n - 1], n' ^ 2 + n'' ^ 2 == n ^ 2]

findPyths :: Int -> [Int]
findPyths n = [n' | n' <- [1 .. n], not (null (pyths n'))]

perfects :: Int -> [Int]
perfects n = [n' | n' <- [1 .. n], sum (init (factors n')) == n']

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]
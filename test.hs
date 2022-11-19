double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

a = b + c
 where
  b = 1
  c = 2

n = a `div` (length xs)
 where
  a = 10
  xs = [1,2,3,4,5]

myLast x = head (reverse x)

myLast1 x = x !! ((length x) - 1)

myInit x = take ((length x) - 1) x

myInit1 :: [Int] -> [Int]
myInit1 x = reverse (tail (reverse x))

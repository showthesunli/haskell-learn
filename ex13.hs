sieve :: [Int] -> [Int]
sieve (n:ns) = n: sieve [x| x<-ns, mod x n /= 0]

primes :: [Int]
primes = sieve [2..]

twins :: [Int] -> [(Int, Int)]
twins ps = [(x, y)| (x, y) <- zip primes (tail primes), x + 1 == y] 
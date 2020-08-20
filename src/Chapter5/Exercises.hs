module Chapter5.Exercises where

-- Exercise 5.1 Sieve of Eratosthenes
-- sieve carries out one step of the SIeve of Eratosthenes. As input it takes a list of integers
-- and filters all multiples of the head of the list (by using filter to return non-multiples)
sieve :: Integral a => [a] -> [a]
sieve ns = filter (\x -> x `mod` (head ns) /= 0) ns

-- Generate all primes by iterating sieve over the infinite list [2 ..]. Each iteration will produce a list
-- in which all multiple of the previous primes have been removed, and the head is the next primes. To produce
-- the infinite list of primes, we have to (map head) over the resulting list of lists.
-- primes !! n will produce the (n+1) prime
-- take n primes will produce the first n primes
primes :: [Integer]
primes = map head $ iterate sieve [2 ..]
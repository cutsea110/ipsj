module Primes where

primes :: [Int]
primes = sieve [2..]
  where sieve (s:ss) = s : sieve (filter (\x -> x `mod` s > 0) ss)

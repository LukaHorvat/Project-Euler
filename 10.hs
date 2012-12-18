main = print . sum . takeWhile (<2000000) $ primes

primes = 2 : 3 : sieve (tail primes) [5,7..]

sieve ps xs = primes ++ sieve (tail ps) [x | x <- rest, rem x (head ps) /= 0]
	where ~(primes,~(_:rest)) = span (< (head ps)^2) xs
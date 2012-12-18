timesDivisibleBy n m
	| n `mod` m == 0 = 1 + timesDivisibleBy (n `div` m) m
	| otherwise = 0
	
primes = [2, 3, 5, 7, 11, 13, 17, 19]
divs = [[timesDivisibleBy n m | m <- primes] | n <- [1..20]]
solution = product (zipWith (\x y -> x ^ y) primes (foldl1 (zipWith max) divs))
isPrime n = not $ any (\m -> n `mod` m == 0) $ [2..(floor . sqrt $ fromIntegral n)]
primes = filter isPrime [2..]

allPrimeDiv :: (Integral a) => a -> [a]
allPrimeDiv m = filter isPrime . filter (\o -> m `mod` o == 0) $ [2..(m `div` 2)]

lpd :: (Integral a) => a -> a
lpd n
	| null divs = n
	| otherwise = lpd (n `div` (head divs))
	where divs = allPrimeDiv n
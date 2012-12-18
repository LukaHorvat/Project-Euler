isPrime n = not $ any (\m -> n `mod` m == 0) $ [2..(floor . sqrt $ fromIntegral n)]
primes = filter isPrime [2..]

solution = primes !! 10000
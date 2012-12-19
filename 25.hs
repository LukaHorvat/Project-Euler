memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)

fib 0 = 0
fib 1 = 1
fib n = memoized_fib (n-2) + memoized_fib (n-1)

solution = (1+) . length . takeWhile (\x -> (length . show $ x) < 1000) . map fib $ [1..]
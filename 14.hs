--The memoized function is not used be I left it in anyways
--It runs out of memory because the numbers get too large. The solution would be
--to memoize only the smaller numbers and not the rest
memo_collatz :: Int -> Int
memo_collatz = (map collatz [1..] !!)
	where
		collatz 1 = 1
		collatz n = if even n then (1 + (memo_collatz (n `div` 2))) else (1 + (memo_collatz (n * 3 + 1)))
		
reg_collatz 1 = 1
reg_collatz n 
	| snd t == 0 	= 1 + reg_collatz (fst t)
	| otherwise		= 1 + reg_collatz (n * 3 + 1)
	where t = quotRem n 2
	
solution = foldl (\(s, m) n -> let x = (reg_collatz n) in (if (x > m) then (n, x) else (s, m))) (0, 0) [1..1000000]

--Problem required compilation
main = print solution
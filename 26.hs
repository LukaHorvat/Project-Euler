import Data.Maybe
import Data.List

fullDiv n m = (r*10) : (fullDiv (r*10) m)
	where (q, r) = quotRem n m
	
numCycle :: [Int] -> [Int] -> Int
numCycle [] _ = 0
numCycle (x:xs) cs
	| i == Nothing	= 1 + (numCycle xs (x:cs))
	| isJust i		= -(length cs)+(fromJust i)+1
	where i = elemIndex x cs
	
solution = snd $ foldl (\(m, i) n -> let c = numCycle (fullDiv 1 n) [] in if (c > m) then (c, n) else (m, i)) (0, 0) [1..1000]

--Takes a few seconds but works
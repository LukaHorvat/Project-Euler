import Data.List
import qualified Data.IntSet as S

primeDivs 1 = []
primeDivs n = p : primeDivs (n `div` p)
	where p = head . filter (\x -> rem n x == 0) $ [2..]
	
groups [] = []
groups ns = p:(groups (drop (snd p) ns))
	where p = (head ns, length (takeWhile (\x -> x == head ns) ns))
	
combo :: (Integral a) => [(a, a)] -> [[a]]
combo [] = []
combo ((p, n):rest) = c:(combo rest)
	where c = map (p^) [0..n]

sumDivisors n = sum . filter (/=n) . map product . sequence . combo . groups . primeDivs $ n
	
abundant = filter (\n -> sumDivisors n > n) [1..20162] --According to Wolfram, this is the real limit

sumOfAbundant = S.fromList [x+y | x <- abundant, y <- abundant, y >= x, x + y <= 20162]

others = S.fromList [1..20162] `S.difference` sumOfAbundant

main = print (sum . S.toList $ others) --Requires compilation
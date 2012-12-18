triangleNumbers = map (\x -> (x + 1) * x `div` 2) [1..]

primeDivs 1 = []
primeDivs n = p : primeDivs (n `div` p)
	where p = head . filter (\x -> rem n x == 0) $ [2..]
	
groups [] = []
groups ns = p:(groups (drop p ns))
	where p = length (takeWhile (\x -> x == head ns) ns)
	
numDivisors n = product g
	where g = map (+1) $ groups (primeDivs n)
	
solution = head . dropWhile (\x -> numDivisors x < 500) $ triangleNumbers

main = print solution
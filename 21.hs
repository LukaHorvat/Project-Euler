sumDiv n = sum . filter (\m -> rem n m == 0) $ [1..quot n 2]
	
solution = sum . filter (\n -> let m = sumDiv n in m < 10000 && n == sumDiv m && m /= n) $ [1..10000]

main = print solution
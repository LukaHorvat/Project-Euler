import Data.List.Split(splitWhen)

fibonacci = 1 : 2 : next fibonacci
	where 
		next (a:b:rest) = (a+b) : next (b:rest)
solution = sum . filter even . takeWhile (<=4000000) $ fibonacci
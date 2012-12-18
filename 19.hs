months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
getDays m y
	| rem y 4 == 0 && m == 1	= 29
	| otherwise					= months !! m

getMonth :: Int -> Int -> [Int]
getMonth y m = 1:(take (getDays m y - 1) . repeat $ 0)

getYear :: Int -> [Int]
getYear y = foldl (\a m -> a ++ (getMonth y m)) [] [0..11]

century :: [Int]
century = foldl (\a y -> a ++ (getYear y)) [] [1..100]

weeks = concat $ repeat (replicate 6 0 ++ [1])
solution = sum $ zipWith (*) century weeks 

--1 too many :(
--Don't know where I made a mistake. I might come back to it later
--Correct solution: 171
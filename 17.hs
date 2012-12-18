numToWord 0 = ""
numToWord 1 = "one"
numToWord 2 = "two"
numToWord 3 = "three"
numToWord 4 = "four"
numToWord 5 = "five"
numToWord 6 = "six"
numToWord 7 = "seven"
numToWord 8 = "eight"
numToWord 9 = "nine"
numToWord 10 = "ten"
numToWord 11 = "eleven"
numToWord 12 = "twelve"
numToWord 13 = "thirteen"
numToWord 14 = "fourteen"
numToWord 15 = "fifteen"
numToWord 16 = "sixteen"
numToWord 17 = "seventeen"
numToWord 18 = "eighteen"
numToWord 19 = "nineteen"
numToWord 20 = "twenty"
numToWord 30 = "thirty"
numToWord 40 = "forty"
numToWord 50 = "fifty"
numToWord 80 = "eighty"
numToWord 1000 = "one thousand"
numToWord n
	| n < 100 && rem n 10 == 0	= numToWord (div n 10)++"ty"
	| n < 100 					= let (q, r) = quotRem n 10 in numToWord (q * 10) ++ (numToWord r)
	| n >= 100 					= let (q, r) = quotRem n 100 in numToWord q ++ " hundred" ++ if (r /= 0) then " and " ++ numToWord r else ""
	
--That was interesting to write...

solution = length . filter (\c -> c /= ' ') . unwords . map numToWord $ [1..1000]
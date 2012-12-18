numbers = [x*y | x <- [100..999], y <- [100..999]]
isPalindrome n = (show n) == (reverse . show $ n)

solution = maximum . filter isPalindrome $ numbers
--Attempting so solve it by hand
--Will include a Haskell solution if I don't manage to do it

--10! permutations in total. Looking for the 1 000 000 th one
--0 and 1 aren't the first. Number starts with 2
--274240th permutation of the rest is needed
--0, 1, 3, 4, 5, 6 aren't the second digit. 7 is.
--32320th permutation of the rest is needed
--0, 1, 3, 4, 5, 6 aren't the third digit. 8 is.
--2080th permutation of the rest is needed
--0, 1 aren't the fourth digit. 3 is.
--640th permutation of the rest is needed
--0, 1, 4, 5, 6 aren't the fifth digit. 9 is.
--0 isn't the sixth digit. 1 is.
--16th permutation of the rest is needed.
--0, 4 aren't the seventh digit. 5 is.
--4th permutation of the rest is needed.
--2783915460 should be the solution.

--AND IT IS
--Let's make a program anyways

fact n = product [1..n]
nthPermutation 1 xs = xs
nthPermutation n xs = (xs !! q):nthPermutation (r+1) (take q xs ++ (drop (q + 1) xs))
	where (q,r) = quotRem (n-1) (fact (length xs - 1))
	
--And there is is
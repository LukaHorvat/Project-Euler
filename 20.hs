import Data.Char

fact n = product [1..n]

solution = sum . map digitToInt . show . fact $ 100
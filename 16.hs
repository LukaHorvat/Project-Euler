--Kind of pointless in Haskell
import Data.Char

solution = sum . map digitToInt . show $ 2^1000
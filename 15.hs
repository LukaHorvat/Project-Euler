import qualified Data.Map as M

routes_map :: M.Map (Int, Int) Integer
routes_map = M.fromList [((x,y),numRoutes x y) | x <- [0..20], y <- [0..20]]

numRoutes 0 0 = 1
numRoutes n m
	| n == 0	= routes_map M.! (n, m-1)
	| m == 0	= routes_map M.! (n-1, m)
	| otherwise	= routes_map M.! (n-1, m) + routes_map M.! (n, m-1)

solution = numRoutes 20 20
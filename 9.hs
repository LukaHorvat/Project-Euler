solution = a * b * c
	where (a, b, c) = 
		head . 
		filter (\(x, y, z) -> x + y + z == 1000) . 
		filter (\(x, y, z) -> z^2 == x^2 + y^2) $ 
		[(x, y, floor . sqrt . fromIntegral $ (x^2 + y^2)) | x <- [1..1000], y <-[1..1000-x]]
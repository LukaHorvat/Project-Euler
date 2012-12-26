odds = filter odd [1..]
topRight = drop 1 . map (^2) $ odds
topLeft = zipWith (\o n -> o - n - 1) topRight odds
bottomLeft = zipWith (\o n -> o - 2 * n - 2) topRight odds
bottomRight = zipWith (\o n -> o - n - 1) bottomLeft odds

solution = (sum . map (sum . take 500) $ [topLeft, topRight, bottomLeft, bottomRight]) + 1
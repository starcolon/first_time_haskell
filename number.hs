scale :: [Integer] -> Integer -> [Integer]
scale [] c = []
scale l 1 = l
scale (a:others) c = a*c : scale others



printl :: [Integer] -> IO()
printl [] = []
printl (a:others) = do
	printl = IO()
	putStr a ++ " => "
	printl others


module Main where
	vector_a = [1,2,3,4,5]
	vector_b = scale vector_a 10
	printl vector_b
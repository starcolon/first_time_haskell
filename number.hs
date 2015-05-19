scale :: [Integer] -> Integer -> [Integer]
scale [] c = []
scale l 1 = l
scale (a:others) c = a*c : scale others



printl :: [Integer] -> IO()
printl [] = []
printl (a:others)  


module Main where
	main :: IO()
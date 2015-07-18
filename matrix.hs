module Main where



identity :: Int -> [[Int]]
identity n 
	| n<=1 = [[1]]
	| otherwise = [[if w==v then 1 else 0|w <- [0..n-1]]|v <- [0..n-1]]



printmat :: [[Int]] -> IO()
printmat [] = return ()
printmat [[]] = return ()
printmat ws = print (head ws) >> printmat (tail ws)



i9 = identity 9

main :: IO()
main = do
	putStr "------------------\n"
	printmat i9
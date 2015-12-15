module Main where

identity :: Int -> [[Int]]
identity n 
	| n<=1 = [[1]]
	| otherwise = [[if w==v then 1 else 0|w <- [0..n-1]]|v <- [0..n-1]]


diag :: Int -> Int -> Int -> Bool
diag a b n
	| a==b = True
	| n-a-1==b = True
	| n-b-1==a = True
	| otherwise = False

star :: Int -> [[Int]]
star n
	| n<=1 = [[0]]
	| otherwise = [[if diag v w n then 0 else 1 | w <- [0..n-1]] | v <- [0..n-1]]


scalerow :: Int -> [Int] -> [Int]
scalerow _ []			= []
scalerow c (w:[])		= [w*c]
scalerow c (w:ws)		= [w*c] ++ scalerow c ws


scale :: Int -> [[Int]] -> [[Int]]
scale _ [[]]			= [[]]
scale c (w:[])			= [scalerow c w]
scale c (w:ws)			= [scalerow c w] ++ scale c ws


printmat :: [[Int]] -> IO()
printmat [] = return ()
printmat [[]] = return ()
printmat ws = print (head ws) >> printmat (tail ws)


i9 = identity 9
c9 = scale 25 i9

u = [[1,3,5],[2,4,4],[1,3,1]]

main :: IO()
main = do
	putStr "------------------\n"
	printmat i9
	putStr "------------------\n"
	printmat c9
	putStr "------------------\n"
	printmat u
	putStr "------------------\n"
	--printmat $ scale 2 u
	printmat $ star 50
	
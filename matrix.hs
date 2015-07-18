module Main where

identity :: Int -> [[Int]]
identity n 
	| n<=1 = [[1]]
	| otherwise = [[if w==v then 1 else 0|w <- [0..n-1]]|v <- [0..n-1]]


addrow :: [[Int]] -> [Int] -> [[Int]]
addrow [[]] [w] 		= [[w]]
addrow [ws] [v] 		= [ws++[v]] 


scalerow :: Int -> [Int] -> [Int]
scalerow _ []			= []
scalerow c (w:[])		= [w*c]
scalerow c (w:ws)		= [w*c] ++ scalerow c ws


scale :: Int -> [[Int]] -> [[Int]]
scale _ [[]]			= [[]]
scale c (w:ws)			= [scalerow c w] ++ scale c ws


printmat :: [[Int]] -> IO()
printmat [] = return ()
printmat [[]] = return ()
printmat ws = print (head ws) >> printmat (tail ws)



i9 = identity 9
c9 = scale 25 i9

main :: IO()
main = do
	putStr "------------------\n"
	printmat i9
	printmat c9
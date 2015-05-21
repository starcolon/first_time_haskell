module Main where

poplist :: [Int] -> (Int,[Int])
poplist [] = (0, [])
poplist (a:aa) = (a,aa)

remove :: [Int] -> Int -> [Int]
remove [] _ = []
remove (a:aa) k
	| a == k = aa
	| otherwise = a : remove aa k

contain :: [Int] -> Int -> Bool
contain [] _ = False
contain (a:aa) k 
	| a == k = True
	| otherwise = contain aa k


divisible_by_other_in :: [Int] -> Int -> Bool
divisible_by_other_in [] _ = False
divisible_by_other_in (a:xs) n
	| a == 1 = divisible_by_other_in (sublist xs n) n 
	| (a < n && mod n a == 0) = True
	| otherwise = divisible_by_other_in (sublist xs n) n

sublist :: [Int] -> Int -> [Int]
sublist xs a = [x|x <- xs, x<a]
	

list :: [Int]
list = [1..4242]
primes = [x|x <- list,  not(divisible_by_other_in list x)]


main :: IO()
main = do
	print primes


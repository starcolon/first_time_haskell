module Main where

triangle_sum :: (Int, Int) -> Int
triangle_sum (x, y) = x*x + y*y

make_triangle_list = 1 : 2 : next make_triangle_list
  where
    next (a : t@(b:_)) = (a*a+b*b) : next t

squared = \x -> x*x


transfer :: ( a -> a ) -> [a] -> [a]
transfer _ [] = []
transfer f (x:[]) = [f x]
transfer f (x:xs) = (f x): transfer f xs 

transfer_tuple :: ( a -> a -> a ) -> [a] -> [a]
transfer_tuple _ [] = []
transfer_tuple f (x:[]) = []
transfer_tuple f (x:xs) = (f x (head xs)): transfer_tuple f xs

secondlast :: [a] -> a
secondlast [] = error "Empty list"
secondlast xs 
	| length xs >= 2 = last (take (l-1) xs)
	| otherwise = last xs
	where l = length xs

append_r :: (Num a) => Int -> ( a -> a -> a ) -> [a] -> [a]
append_r _ _ [] = []
append_r 0 _ xs = xs 
append_r _ f (x:[]) = [x]
append_r n f xs 
	| length xs >= 2 = 
		let {lst = last xs; lst2 = secondlast xs}
		in append_r (n-1) f (xs ++ [f lst lst2])
	| otherwise = xs



--list_tri = take 10 make_triangle_list
--list_u = transfer (\x -> x*5) [1,2,3]
list_s = [1..2]
--list_u = transfer_tuple (*) list_s
list_mul = append_r 7 (*) list_s
list_diff_sqr = append_r 7 (\x y -> x*x - y*y) list_s

main :: IO()
main = do
	putStr "Mapping from ..."
	print list_s
	putStr "To ..."
	print list_mul
	print list_diff_sqr

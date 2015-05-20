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

--list_tri = take 10 make_triangle_list
list_u = transfer (\x -> x*5) [1,2,3]


main :: IO()
main = do
	putStr "Lorem ipsom dolor"
	--print list_tri
	print list_u


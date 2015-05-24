module Main where

	-- Zip two lists together with a mapping function a,a -> a
	zipfn :: [a] -> (a -> a -> a) -> [a] -> [a]
	zipfn (x:[]) f (y:_) = [f x y]
	zipfn (x:xs) f (y:ys) = [f x y] ++ (zipfn xs f ys)


	regulate :: [Int] -> [Int]
	regulate [] = []
	regulate (x:[]) 
		| x>=5 = [1]
		| otherwise = [0]
	regulate (x:xs) = (regulate (x:[])) ++ (regulate xs)

	printpulse :: [Int] -> IO()
	printpulse [] = return ()
	printpulse (x:[]) = return ()
	printpulse (x:(y:ys)) 
		| x==1 && y==0 = putStrLn "  |" >> putStrLn "--" >> printpulse (y:ys)
		| x==1 && y==1 = putStrLn "  |" >> putStrLn "  |" >> printpulse (y:ys)
		| x==0 && y==0 = putStrLn "| " >> putStrLn "| " >> printpulse (y:ys)
		| otherwise = putStrLn "| " >> putStrLn "--" >> printpulse (y:ys)

	modulate = \v w -> zipfn v (\x y -> x*y) w
	combine = \v w -> zipfn v (\x y -> max x y) w



	pulse1 = [0,3,8,11,13,15,10,7,7,3,1,0,5,6,0,0,7,5,5,3,5,6,1,6,6,1,0,5,5,2]
	pulse2 = [0,1,1,5,3,8,10,10,12,8,8,3,0,0,0,1,0,0,1,5,7,0,0,1,15,3,8,9]
	bits1 = regulate pulse1
	bits2 = regulate pulse2

	modbits = modulate bits1 bits2
	orbits = combine bits1 bits2


	main :: IO()
	main = do
		putStrLn "========================================"
		printpulse bits1
		putStrLn " - modulate with -"
		printpulse bits2
		putStrLn " - results in -"
		printpulse orbits
		putStrLn "========================================"
		


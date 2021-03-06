module Main where

data Vector3D = Vector3D Float Float Float

cross :: Vector3D -> Vector3D -> Float
cross (Vector3D u v w) (Vector3D x y z) = u*x + v*y + w*z

sqrdiff :: Float -> Float -> Float
sqrdiff f1 f2 = (f1-f2)^2

dist :: Vector3D -> Vector3D -> Float
dist (Vector3D u v w) (Vector3D x y z) = sqrt $ dx+dy+dz
	where {dx = sqrdiff u x;
			dy = sqrdiff v y;
			dz = sqrdiff w z}

mag :: Vector3D -> Float
mag (Vector3D u v w) = sqrt $ u2+v2+w2
	where {u2 = u*u; 
			v2 = v*v;
			w2 = w*w}

dot :: Vector3D -> Vector3D -> Float
dot (Vector3D u v w) (Vector3D x y z) = u*x + v*y + w*z

scale :: Float -> Vector3D -> Vector3D
scale c (Vector3D u v w) = (Vector3D cu cv cw)
	where {cu = c*u;
			cv = c*v;
			cw = c*w}

exert :: [Vector3D] -> (Vector3D -> Vector3D) -> [Vector3D]
exert [] _ = []
exert [v] t = [t v] 

mapmag :: [Vector3D] -> [Float]
mapmag [] = []
mapmag (v:[]) = [mag v]
mapmag (v:vs) = [mag v] ++ mapmag vs

addvec :: Vector3D -> [Vector3D] -> [Vector3D]
addvec v [] = [v]
addvec v ws = ws++[v]

printv :: Vector3D -> IO()
printv (Vector3D u v w) = putStr "(" >> print u >> print v >> print w >> putStr ")"

printvs :: [Vector3D] -> IO()
printvs [] = return ()
printvs (v:[]) = print v
printvs (v:vs) = print v >> printvs vs


-- sequence of process
my_field = [] >>= addvec (Vector3D 0 0 0)



main :: IO()
main = do
	putStrLn "================================="




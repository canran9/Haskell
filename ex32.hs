reverse [ ] = [ ]
reverse (x:xs) = reverse xs ++ [x]

Methode2
reverse' r [] = r
reverse' r (x:xs) = reverse' (x:r) xs

reverse = reverse' []

delete _ [] = [] 
delete y (x:xs) = if x ==y
		then xs
		else x : delete y xs

maximum [] = error "..."
maimum [x] = x
maximum ( x:xs ) = max x (maximum xs)

trimax [] = []
trimax xs = let x = maximum xs
	   in x : trimax (delete x xs)
  
import Data.List   (pour delete & sort)
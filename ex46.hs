sum xs = foldl (+) 0 xs

maximum xs = foldl1 max xs

and xs = foldl (&&) True xs

any f xs = or $ map f xs 
or xs = fold (||) False xs

concat xs = foldl (++)  [] xs


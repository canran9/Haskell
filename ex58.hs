import Data.char

cesar :: Interger -> String -> String
cesar _ [] = []
cesar n (x : xs) = chr ((ord x-a + n) mod 26) : cesar n xs

或 cesar x ys = let a = ord 'a'
	     in map (\y -> chr(mod(ord y - a + x) 26) + a)

main = do
      n <- getLine
      s <- getLine
      putStrLn $ cesar (read n) s  
      
getIntBorne :: IO Int
getIntBorne = do
           putStrLn "Entrez une valeur ..."
           x <- getLine
           let n = (read x) :: Int
           if n >= 0 && n< 25 then return n
		            else getIntBorne  
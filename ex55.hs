import Data.List

main = getLine >>= (\x -> putStrLn( x ++ reverse x ))

main = getLine >>= (\ x -> getLine
                        >>= (\ y -> putStrLn ( y ++ " " ++ x )))
	          

 

 
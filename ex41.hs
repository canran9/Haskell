an "abc" -> ["abc", "acb", "bac", "bca", "cab", "cba"]
an :: String -> [ String ]
        [Char]

an :: [a] -> {a} 
an [] = [ [] ] 
an (x:xs) = concat (map (between x) (an xs)) 
    where between e [] = [ [e] ] 
          between e (y:ys) = (e:y:ys) : map (y:) (between e ys) 




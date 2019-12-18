map _ [] = []
map f (x : xs ) = (f x ):( map f xs )

map ::  (a ->b) -> [a] ->[b]

flip :: (a ->b ->c) -> (b ->a ->c)
flip           f                 x     y    =    x      f y x 
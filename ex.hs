r' 0 x = x
r' n x = r' (div n 10) (x*10 + mod n 10)
r n = r' n 3

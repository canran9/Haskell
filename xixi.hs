data Nat = Zero | Succ Nat
initVal :: Nat -> Integer
initVal Zero = 0
initVal (Succ x) = (initVal x)+1

 
add Zero y = y
add (Succ x) y = Succ (add x y)
	           add x (Succ y)

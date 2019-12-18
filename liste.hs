data Liste = Vide | Cons Interger Liste

(Cons 4 (Cons 3 (Cons 2 Vide)))
sum Vide = 0
sum (Cons x xs) = x+sum xs
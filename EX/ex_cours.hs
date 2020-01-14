-----------------------------
---------- Diapo 15 ---------
-----------------------------
-- 15.1.1
-- puiss x 0 = 1
-- puiss x n = x * (puiss x (n-1))

-- main = print (puiss 3 1)


-- 15.1.2
-- puiss x n = case n of 
--     0 -> 1
--     _ -> if (even n)
--         then puiss (x*x) (n `div` 2) 
--         else x * (puiss (x*x) (n `div` 2))

-- main = print (puiss 4 3)

-- 15.2.1
-- fibo 0 = 0
-- fibo 1 = 1
-- fibo n = (fibo (n-1)) + (fibo (n-2))

-- main = print (fibo 6)
-- Complexité temporelle mauvaise


-----------------------------
---------- Diapo 16 ---------
-----------------------------
-- r::Integer->Integer
-- raux 0 acc = acc
-- raux n acc = raux (div n 10) (10*acc + (mod n 10))
-- r n = raux n 0

-- hpal n = if (r n) == n 
--     then 0 
--     else (1 + hpal (n + r n))

-- main = print(hpal 123)


-----------------------------
---------- Diapo 22 ---------
-----------------------------
-- et::Bool -> Bool -> Bool
-- et True True = True
-- et _ _ = False
-- main = print(et True False)


-----------------------------
---------- Diapo 23 ---------
-----------------------------
-- data Point = Coord Double Double -- Coord est le constructeur de donnée

-- distance (Coord x1 y1) (Coord x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
-- main = print(distance (Coord 0.0 0.0) (Coord 1.0 1.0))

-- data Figure = FigP Point | FigCe Point Double | FigCa Point Point

-- perimetre::Figure -> Double
-- perimetre (FigP _) = 0.0
-- perimetre (FigCe (Coord _ _) r) = 2 * pi * r
-- perimetre (FigCa p p') = distance p p' * 2 * sqrt 2


-----------------------------
---------- Diapo 28 ---------
-----------------------------
-- data Nat = Zero | Succ Nat
-- intVal::Nat -> Integer
-- intVal Zero = 0
-- intVal (Succ x) = (intVal x) + 1

-- addition::Nat -> Nat -> Nat
-- addition Zero n = n -- on va faire décroitre le premier argument jusqu'à zéro tout en augmentant le deuxième
-- addition (Succ x) y = addition x (Succ y) -- ou Succ (addition x y)

-- main = print(intVal(addition (Succ (Succ Zero)) (Succ Zero))) -- cancer


-----------------------------
---------- Diapo 29 ---------
-----------------------------
-- data Liste = Vide | Cons Integer Liste

-- somme::Liste -> Integer
-- somme Vide = 0
-- somme (Cons x l) = x + somme l

-- data ArbreBinaire = VideA | Noeud Integer ArbreBinaire ArbreBinaire
-- hauteur VideA = 0
-- hauteur (Noeud x g d) = 1 + max (hauteur g) (hauteur d)

-- main = print "ok"


-----------------------------
---------- Diapo 31 ---------
-----------------------------
-- trip = [(a,b,c) | a<-[0..333], b<-[(a+1)..500], c<-[(b+1)..1000], a+b+c==1000, a^2+b^2==c^2]
-- main = print trip
-- résultat : [(200,375,425)]


-----------------------------
---------- Diapo 32 ---------
-----------------------------
-- 1
-- reverseAux [] acc = acc
-- reverseAux (x:xs) acc = reverseAux xs (x:acc)

-- rev x = reverseAux x []

-- main = print (rev [1..10])

-- 2.1
-- deleteL [] e = []
-- deleteL (x:xs) e = if x==e then xs else x:(deleteL xs e)
-- -- main = print(deleteL [1..10] 11)

-- -- 2.2
-- maxL_aux [] x = x
-- maxL_aux (x:xs) m = if x > m then maxL_aux xs x else maxL_aux xs m

-- maxL::[Integer]->Integer-- obligé de spécifier le type pour que maxL [] marche
-- maxL [] = error "maxL: La liste est vide"
-- maxL (x:xs) = maxL_aux xs x
-- -- or :
-- -- maxL [x] = x
-- -- maxL (x:xs) = maxL x (maxL xs)

-- main = print(maxL [1,5,3,2])
--main = print(maxL [])

-- 2.3
-- trimax_aux [] acc = acc
-- trimax_aux x acc = let m = maxL x in trimax_aux (deleteL x m) (m:acc)
-- trimax x = trimax_aux x []

-- main = print(trimax [1,5,3,2,7,8,6])

-----------------------------
---------- Diapo 37 ---------
-----------------------------
-- 1. 
-- map::(a->b)->[a]->[b]

-- 2.
-- myflip::(a->b->c)->(b->a->c)

-- myflip' f x y = f y x
-- myflip f = myflip' f

-- main = print(myflip (-) 1 3) -- output = 2


-----------------------------
---------- Diapo 41 ---------
-----------------------------
-- "abc" = ["a", "b", "c"] et "" = []. String = [Char] (alias)
-- import Data.List -- pour delete
-- an::String->[String]
-- an "" = [""]
-- an xs = concat $ map (\c -> map (c:) (an (delete c xs))) xs

-- main = print(an "abc")


-----------------------------
---------- Diapo 42 ---------
-----------------------------
-- divise n k = mod n k == 0 -- True if k|n
-- diviseurs n = filter (divise n) [1..n]
-- diviseurs n = filter ((==0).(mod n)) [1..n] -- Le point est la composition mathématique

-- premiers n = filter (\x -> length (diviseurs x) == 2) [2..n]

-- main = print(premiers 15)


-----------------------------
---------- Diapo 46 ---------
-----------------------------
-- 1
-- sumL xs = foldl (+) 0 xs
-- ou 
-- sumL = foldl (+) 0
-- main = print(sumL [1,2,3])

-- 2
-- maxL::Ord a => [a] -> a
-- maxL = foldl1 max
-- main = print(maxL [1,5,3])

-- 3
-- andL = foldr (&&) True
-- main = print(andL [True, False, True])

-- 4
-- f x = x == 0 -- prédicat de test
-- anyL f xs = or $ (map f xs)
-- or xs = foldr (||) False xs
-- main = print(anyL f [1,2,0,3,4])

-- 5
-- concatL = foldl (++) []
-- main = print(concatL [[1,2], [3,4,5], [6]])


-----------------------------
---------- Diapo __ ---------
-----------------------------
-- diviseurs n = filter ((==0).(mod n)) $ takeWhile (not.(>n).(^2)) primes
-- primes = 2:filter (null.diviseurs) [3..] 
-- null renvoie True si la liste est vide. On sort 2 pour avoir un cas de base






deleteL [] e = []
deleteL (x:xs) e = if x==e then xs else x:(deleteL xs e)

maxL_aux [] x = x
maxL_aux (x:xs) m = if x > m then maxL_aux xs x else maxL_aux xs m

maxL::[Integer]->Integer-- obligé de spécifier le type pour que maxL [] marche
maxL [] = error "maxL: La liste est vide"
maxL (x:xs) = maxL_aux xs x

trimax_aux [] acc = acc
trimax_aux x acc = let m = maxL x in trimax_aux (deleteL x m) (m:acc)
trimax x = trimax_aux x []

main = print(trimax [1,5,3,2,7,8,6]

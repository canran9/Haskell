-----------------------------
---------- Diapo 64 ---------
-----------------------------
-- 1
-- import Data.Maybe
-- data Expr a = Val a | Inc (Expr a) | Dec (Expr a) | Inv (Expr a) | Neg (Expr a)

-- 2
-- evaluate::(Fractional a) => Expr a -> a
-- evaluate (Val a) = a
-- evaluate (Inc e) = evaluate e + 1
-- evaluate (Dec e) = evaluate e - 1
-- evaluate (Inv e) = 1 / (evaluate e)
-- evaluate (Neg e) = - (evaluate e)

-- 3
-- main = print $ evaluate $ Dec(Neg(Inc(Val 4)))
-- main = print $ evaluate $ Inv(Val(0))

-- 4
-- isZero::(Eq a) => (Num a) => (Maybe a) -> Bool
-- isZero Nothing = False
-- isZero (Just x) = (x == 0)

-- mevaluate::(Eq a) => (Fractional a) => Expr a -> Maybe a
-- mevaluate (Val x) = Just x
-- mevaluate (Inc e) = let res = mevaluate e in if isNothing res then Nothing else Just (fromJust res + 1)
-- mevaluate (Dec e) = let res = mevaluate e in if isNothing res then Nothing else Just (fromJust res - 1)
-- mevaluate (Inv e) = let res = mevaluate e in if (isNothing res) || (isZero res) then Nothing else Just (1/ (fromJust res))
-- mevaluate (Neg e) = let res = mevaluate e in if isNothing res then Nothing else Just (negate (fromJust res))

-- main = print $ mevaluate $ Inv(Val(0))

-- isZero::(Eq a) => (Num a) => (Maybe a) -> Bool
-- isZero Nothing = False
-- isZero (Just x) = (x == 0)

-- mfmap::(a->b) -> Maybe a -> Maybe b
-- mfmap _ Nothing = Nothing
-- mfmap op (Just x) = Just (op x)

-- mevaluate::(Eq a) => (Fractional a) => Expr a -> Maybe a
-- mevaluate (Val x) = Just x
-- mevaluate (Inc e) = mfmap (+1) (mevaluate e)
-- mevaluate (Dec e) = mfmap (subtract 1) (mevaluate e)
-- mevaluate (Inv e) = let res = mevaluate e in if isZero res then Nothing else mfmap (\x -> 1/x) res
-- mevaluate (Neg e) = mfmap negate (mevaluate e)

-- main = print $ mevaluate $ Inv(Val(0))


-----------------------------
---------- Diapo 65 ---------
-----------------------------
-- q2
-- myfmap::(a->b)->[a]->[b] -- même signature que map
-- myfmap f [] = []
-- myfmap f (x:xs) = (f x):(fmap f xs)

-- q3
-- myfmap::(a->b)->(r->a)->(r->b)
-- myfmap f g = f.g



-----------------------------
---------- Diapo 66 ---------
-----------------------------
-- import Data.Maybe
-- data Expr a = Val a | Add (Expr a) (Expr a) | Sub (Expr a) (Expr a) | Mul (Expr a) (Expr a) | Div (Expr a) (Expr a) deriving Show

-- mliftA2::(a->b->c)->Maybe a->Maybe b->Maybe c
-- mliftA2 _ Nothing _ = Nothing 
-- mliftA2 _ _ Nothing = Nothing
-- mliftA2 f (Just a) (Just b) = Just $ f a b

-- isZero::(Eq a) => (Num a) => (Maybe a) -> Bool
-- isZero Nothing = False
-- isZero (Just x) = (x == 0)

-- mevaluate::(Eq a) => (Fractional a) => Expr a -> Maybe a
-- mevaluate (Val x) = Just x
-- mevaluate (Add e1 e2) = mliftA2 (+) (mevaluate e1) (mevaluate e2)
-- mevaluate (Sub e1 e2) = mliftA2 (-) (mevaluate e1) (mevaluate e2)
-- mevaluate (Mul e1 e2) = mliftA2 (*) (mevaluate e1) (mevaluate e2)
-- mevaluate (Div e1 e2) = let res2 = mevaluate e2 in 
--     if isZero res2 then Nothing
--     else mliftA2 (/) (mevaluate e1) res2


-----------------------------
---------- Diapo 68 ---------
-----------------------------
-- q1
apm::Maybe(a->b)->Maybe a->Maybe b
apm Nothing _ = Nothing
apm _ Nothing = Nothing
apm (Just f) (Just a) = Just $ f a

-- q2
import Data.Maybe
data Expr a = Val a | Add (Expr a) (Expr a) | Sub (Expr a) (Expr a) | Mul (Expr a) (Expr a) | Div (Expr a) (Expr a) | Add3 (Expr a) (Expr a) (Expr a) deriving Show

mfmap::(a->b) -> Maybe a -> Maybe b
mfmap _ Nothing = Nothing
mfmap op (Just x) = Just $ op x

mevaluate (Add e1 e2) = apm $ mfmap ((+) (mevaluate e1)) (mevaluate e2)

-- q3/4
add3 x y z = x + y + z
mevaluate (Add3 e1 e2 e3) = mfmap add3 e1 `apm` e2 `apm` e3


-----------------------------
---------- Diapo 70 ---------
-----------------------------
fmap::(a->b) -> m a -> m b
pure::a -> m a
ap::m (a->b) -> m a -> m b
fmap f x = ap (pure f) x -- pure f <*> x


-----------------------------
---------- Diapo 71 ---------
-----------------------------
pure::a->(r->a) 
pure x = (\_ -> x)

(<*>)::(r->(a->b))->(r->a)->(r->b)
-- exemple : f <*> u = (\x -> f x (u x))

f = (*) <*> cos
g = pure (*) <*> cos <*> sin -- redécomposer les fonctions pour comprendre


-----------------------------
---------- Diapo 75 ---------
-----------------------------
return::a-> ma
(>>=)::m a -> (a -> mb) -> m b

fmap::(a->b)->(m a)->(m b)
fmap f x = x >>= return.f 

class Expr a = Val a | Inc (Expr a) | Dec (Expr a) | Inv (Expr a) | Neg (Expr a)

evaluate :: (Fractional a, Eq a) => Expr a -> a
evaluate (Val x) = x
evaluate (Inc y) = evaluate y + 1
evaluate (Dec y) = evaluate y - 1
evaluate (Inv y) = 1/evaluate y
evaluate (Neg y) = negate (evaluate y)

main = print $ evaluate $ Dec $ Neg $ Inc $ Val 4

isZero :: （Num a, Eq a) => Maybe a -> Bool
isZero Nothing = False
isZero (Just x) = (x == 0)    

Data.Maybe
isNothing
isJust
fromJust

mevaluate :: (Fractional a, Eq a) => Expr a -> Maybe a
mevaluate (Val x) = Just x
mevaluate (Inc y) = let z = mevaluate y in 
		if isNothing z then Nothing 
		else Just( FromJust z +1)
mevaluate (Dec y) = let z = mevaluate y in Just(FrmJust - 1)
mevaluate (Inv y) = let z = mevaluate y in 
		if isZero z || isNothing z then Nothing 
		else Just (1/fromJust z)
mevaluate (Neg y) = let z = mevaluate y in Just (negate $ fromJust z)
                                                                                                                          mfmap :: (a -> b) -> Maybe a -> Maybe b
mfmap         _           Nothing = Nothing
mfmap         f            (Just w) =  Just $ f w

modification:
mevaluate (Inc y) = mfmap (+1) $ evaluate y
                  (Dec y) = mfmap (subtract 1) $ evaluate y
                   (Inv y) = let z = evaluate y in if isZero z then Nothing else mfmap (1/) z
                    (Neg y) = mfmap negate $ evaluate y









  
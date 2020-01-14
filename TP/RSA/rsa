import Data.Char

data Message = Message [Integer] deriving Show

stringToMessage str = Message(map (fromIntegral.ord) str)
messageToString (Message msg) = map (chr.fromIntegral) msg
-- main = print $ messageToString $ stringToMessage "hello world"

pad size (Message msg) = let r = size - (mod (length msg) size) in 
                    if r == 0 then
                        Message(msg ++ replicate (fromIntegral size) (fromIntegral size))
                    else
                        Message(msg ++ replicate (fromIntegral r) (fromIntegral r))
-- main = print $ pad 5 (stringToMessage "hello world")

unpad'::[Integer]->Int->[Integer]
unpad' msg 0 = msg
unpad' [] _ = []
unpad' (x:xs) padsize = unpad' xs (padsize - 1)
unpad::Message->Message
unpad (Message []) = (Message [])
unpad (Message msg) = let (padSize:xrevs) = reverse msg in Message(reverse (unpad' (padSize:xrevs) (fromIntegral padSize)))
-- main = print $ messageToString $ unpad (pad 5 (stringToMessage "hello world"))


groupBytes::[Integer]->Integer
groupBytes [] = 0
groupBytes (x:xs) = x*256^(length xs) + groupBytes xs
-- main = print $ groupBytes [128, 54, 33, 99]

ungroupBytes::Int->Integer->[Integer]
ungroupBytes 1 n = [n]
ungroupBytes bsize n = let a = (256^(bsize - 1)) in let q = (n `div` a) in q:(ungroupBytes (bsize - 1) (n `mod` a))
-- main = print $ ungroupBytes 4 2151031139

groupN::Int->[Integer]->[[Integer]]
groupN bsize [] = []
groupN bsize m = let (group, rem) = splitAt bsize m in [group] ++ (groupN bsize rem)
-- main = print $ groupN 3 [1,2,3,4,5,6,7,8,9]

makeBlocks::Int->Message->Message
makeBlocks bsize (Message msg) = Message(map groupBytes (groupN bsize msg))
-- main = print $ makeBlocks 3 (stringToMessage "hello world1")

splitBlocks::Int->Message->Message
splitBlocks bsize (Message msg)= Message(concat (map (ungroupBytes bsize) msg))
-- main = print $ splitBlocks 3 (makeBlocks 3 (stringToMessage "hello world1"))

prime::Integer->Bool
prime 2 = True
prime 3 = True
prime n = not $ any (\k -> (mod n k == 0)) (2:3:[6*k-1 | k<-[1..n], (6*k-1)^2 <= n] ++ [6*k+1 | k<-[1..n], (6*k+1)^2 <= n])
-- main = print $ prime 31

choosePrime b = head $ dropWhile (not.prime) [b..]
-- main = print $ choosePrime 128

eucl r u v 0 u' v' = (r, u, v)
eucl r u v r' u' v' = eucl r' u' v' (r - (r `div` r') * r') (u - (r `div` r')*u') (v - (r `div` r')*v') 
euclid a b = eucl a 1 0 b 0 1
-- main = print $ euclid 25 15 

modInv e n = let (_, u, _) = euclid e n in if u >= 0 then u else u + n -- u = d dans l'énoncé quand n = phi

modExp::Integer->Integer->Integer->Integer
modExp 0 _ _ = 0
modExp _ 0 _ = 1
modExp m e n =  -- calcul de m^e (mod n)
    if even e then
        let y = modExp m (e `div` 2) n in (y * y) `mod` n
    else
        ((m `mod` n) * (modExp m (e-1) n)) `mod` n
-- main = print $ modExp 7 256 13 -- 9

encrypt::Integer->Integer->Int->String->Message
encrypt e n bsize str = let (Message blocks) = makeBlocks bsize (pad bsize (stringToMessage str)) in let msg = map (\x -> modExp x e n) blocks in Message(msg)
-- main = print $ encrypt 12 4 3 "hello world"

decrypt::Integer->Integer->Int->Message->String
decrypt d n bsize (Message msg) = messageToString $ unpad $ splitBlocks bsize (Message(map (\x -> modExp x d n) msg))


-- génère p >= min tq e ne divise pas (p-1)
genPrime min e = let p = choosePrime min in 
    if mod (p-1) e == 0 then genPrime (p+1) e
    else p

-- génère deux entiers premiers p et q (q > p) tq e ne divise pas (p-1)(q-1)
genBase e = let p = genPrime (e+1) e in
    let q = genPrime (p+1) e in (p,q)



-- test
str = "hello world!"
e = 65537
(p,q) = genBase e
n = p*q
phi = (p-1)*(q-1)
d = modInv e phi

-- les blocs de taille >= 5 ne marchent pas car les entiers en base 256 sont >= n 
-- ajouter bsize dans genBase pour que n = p*q soit supérieur à 256^bsize ?
encrypted = encrypt e n 4 str
decrypted = decrypt d n 4 encrypted
main = print decrypted

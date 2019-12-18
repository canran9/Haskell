import Data.Char
import Data.List

data Message = Vide | Mes [Integer] deriving (Show)


stringToMessage :: String -> Message
stringToMessage x = Mes $ map (fromIntegral.ord) x


messageToString :: Message -> String
messageToString (Mes x) = map (chr.fromIntegral) x

pad :: Int -> Message -> Message
pad n (Mes x) = if n > length x then let l = n - (mod (length x) n)
                                   in Mes (x ++ (replicate l (fromIntegral l)))
                else Mes x
--main = do
  --  let a = stringToMessage "123" in print a

--main = do
  --  let a = messageToString (Mes [123]) in print a

lengthCheck :: Int -> Message -> Maybe Message
lengthCheck n (Mes x) = if extra x /= 0
then Nothing
else Just (Mes x)
  where extra = (mod n) . length

validPadChar :: Int -> Message -> Maybe Message
validPadChar n (Mes x) = if (fromIntegral $ last x) <= n
                              then Just (Mes x)
                              else Nothing
dropPadding :: Message -> Maybe Message
dropPadding (Mes x) = if all (== padInteger) p
                   then Just (Mes m)
                   else Nothing
  where padInteger = last x
        leading = length x - fromIntegral padInteger
        (m, p)  = splitAt leading x
  
unpad :: Int -> Message -> Maybe Message
unpad n (Mes x) = lengthCheck n (Mes x)
                 >>= validPadChar n
                 >>= dropPadding

groupBytes :: Message -> Integer
groupBytes (Mes x) = foldl' (+) 0 [(x !! (finL - y))*256^y | y <- [0..finL]]
     where finL = length x - 1

ungroupBytes :: Int -> Integer -> Message
ungroupBytes bsize bloc = Mes $ reverse $ tail (iterate (mod 256) bloc)











main = do
      let a = ungroupBytes 4 2151031139 in print a
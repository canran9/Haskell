ioLength : : IO Int
ioLength = do
	x <- getLine
	return $ length x

ioLength = getLine >>= return.length

main = ioLength >>= print
main = do
           x <- ioLength
           print x
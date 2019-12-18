diviseurs :: Interger -> [Interger]
//diviseurs n = [a | a<- [ . . n ], rem n a == 0 ]
diviseurs n = filter ((==0).(  mod n)) [1 . . n]


premiers :: Interger -> [Interger]
premiers n = filter ((==2).length.diviseurs) [1 . . n]


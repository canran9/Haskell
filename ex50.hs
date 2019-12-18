
diviseurs n = filter ((==0).(  mod n)) $ tekeWhile (not.(>n).(^2)) premiers

premiers = 2 : filter (null.diviseurs) [3 . . ]

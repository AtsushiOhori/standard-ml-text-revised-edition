open Prime InfList
val nats = FROMN 0
val evens = FILTER (fn x => x mod 2 = 0) nats
val a = NTH 1000 nats
val b = NTH 1000 evens
val c = VIEW (1000, 5) PRIMES
val _ = Dynamic.pp {a = a, b = b, c = c}

structure Prime =
struct
  open InfList
  fun SIFT NIL = NIL
    | SIFT L = 
      let val a = HD L
      in CONS (a, fn () => 
                  SIFT (FILTER (fn x => x mod a <> 0) (TL L)))
      end
  val PRIMES = SIFT (FROMN 2)
end

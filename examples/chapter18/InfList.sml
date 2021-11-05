structure InfList = 
struct
  datatype 'a inflist
    = NIL
    | CONS of 'a * (unit -> 'a inflist);
  fun HD (CONS (a, b)) = a
  fun TL (CONS (a, b)) = b ()
  fun NULL NIL = true | NULL _ = false
  fun FILTER f l = if NULL l then NIL
                   else if f (HD l) then 
                     CONS (HD l, fn () => (FILTER f (TL l)))
                   else FILTER f (TL l)
  fun NTH 0 L = HD L
    | NTH n L = NTH (n - 1) (TL L)
  fun FROMN n = CONS (n, fn () => FROMN (n+1))
  fun DROP 0 L = L
    | DROP n L = DROP (n - 1) (TL L);
  fun TAKE 0 L = nil
    | TAKE n L = HD(L) :: TAKE (n - 1) (TL L);
  fun VIEW (n,m) L = TAKE m (DROP n L);
end

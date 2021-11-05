smlsharp <<EOF
datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;
Empty;
Node;
Node (1,Empty,Empty);
Node ((fn x => x),Empty,Empty);
Node("a", Node("b",Empty,Empty),
          Node("c",Node("d",Empty,Empty),Empty));
fun decompose s =
    let 
      fun searchLP s p = 
          if substring(s,p,1) = "(" then p
          else searchLP s (p+1) 
               
      fun searchRP s p n = 
          case substring(s,p,1) of
            "(" => searchRP s (p+1) (n+1) 
          | ")" => if n=0 then p else searchRP s (p+1) (n - 1)
          | _ => searchRP s (p+1) n
      val lp1 = searchLP s 0 
      val rp1 = searchRP s (lp1+1) 0
      val lp2 = searchLP s (rp1+1)
      val rp2 = searchRP s (lp2+1) 0
    in (substring (s,0,lp1),
        substring (s,lp1+1,rp1-lp1-1),
        substring (s,lp2+1,rp2-lp2-1))
    end;
fun fromPreOrder s =
    let fun decompose s =
          let 
            fun searchLP s p = 
                if substring(s,p,1) = "(" then p
                else searchLP s (p+1) 
                     
            fun searchRP s p n = 
                case substring(s,p,1) of
                  "(" => searchRP s (p+1) (n+1) 
                | ")" => if n=0 then p else searchRP s (p+1) (n - 1)
                | _ => searchRP s (p+1) n
            val lp1 = searchLP s 0 
            val rp1 = searchRP s (lp1+1) 0
            val lp2 = searchLP s (rp1+1)
            val rp2 = searchRP s (lp2+1) 0
          in (substring (s,0,lp1),
              substring (s,lp1+1,rp1-lp1-1),
              substring (s,lp2+1,rp2-lp2-1))
          end
    in if s = "" then Empty 
       else let val (root,left,right) = decompose s
            in Node(root,fromPreOrder left,fromPreOrder right)
            end
    end;

case Node ("Joe",Empty,Empty) of Empty => "empty"
                               | Node (x,_,_) => x;

fun max(a, b) = if a > b then a else b;
fun height t =
     case t of Empty => 0
             | Node (_,t1,t2) => 1 + max(height t1, height t2);
fun height Empty = 0
  | height (Node(_,t1,t2)) = 1 + max(height t1, height t2);

fun f Empty = true
  | f (Node(_,Empty,Empty)) = true
  | f (Node(_,x as Node _, y as Node _)) = f x andalso f y
  | f _ = false
fun fib 0 = 1
  | fib 1 = 1
  | fib n = fib (n - 1) + fib (n - 2);

val Node(x,y,z) =  Node("a",Node("b",Empty,Empty),Empty);
val Empty = Node("b", Empty, Empty);

type 'a dict = (string * 'a) tree
fun enter (key,v,dict) =
    case dict of 
      Empty => Node((key,v),Empty,Empty)
    | Node(node as (key',_),L,R) => 
      if key = key' then dict 
      else if key > key' then 
        Node(node, L, enter (key,v,R))
      else Node(node, enter (key,v,L),R)
fun lookUp (key,Empty) = NONE
  | lookUp (key,Node((key',v),L,R)) = 
    if key = key' then SOME v
    else if key > key' then lookUp (key,R)
    else lookUp (key,L)

datatype 'a inflist  = NIL | CONS of 'a * (unit -> 'a inflist);
fun FROMN n = CONS(n,fn () => FROMN (n+1));
FROMN 1;
val CONS(x,y) = it;
y ();

fun HD (CONS(a,b)) = a;
fun TL (CONS(a,b)) = b();
fun NULL NIL = true | NULL _ = false;
val naturalNumbers = FROMN 0;
HD naturalNumbers;
TL naturalNumbers;
HD (TL(TL(TL it)));

fun NTH 0 L = HD L
  | NTH n L = NTH (n - 1) (TL L);

NTH 100000000 naturalNumbers;

fun filter f l = if null l then nil
                 else if f (hd l) then 
                   hd l :: (filter f (tl l))
                 else filter f (tl l);

fun FILTER f l = if NULL l then NIL
                 else if f (HD l) then 
                   CONS(HD l,fn () => (FILTER f (TL l)))
                 else FILTER f (TL l);

fun SIFT NIL = NIL
  | SIFT L = 
    let val a = HD L
    in CONS(a, fn () => 
                  SIFT (FILTER (fn x => x mod a <> 0) (TL L)))
    end;
val PRIMES = SIFT (FROMN 2);
fun DROP 0 L = L
  | DROP n L = DROP (n - 1) (TL L);
fun TAKE 0 L = nil
  | TAKE n L = HD(L) :: TAKE (n - 1) (TL L);
fun VIEW (n,m) L = TAKE m (DROP n L);
VIEW (10000,5) PRIMES;
EOF

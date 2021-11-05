smlsharp<<EOF
val x = ref 1;
!x;
x:=2;
!x;

local 
   val state = ref nil : int list ref
   fun next nil = [ord #"a"]
     | next (h::t) = if h = ord #"z" then 
                        ord #"a" :: (next t)
                     else (h+1::t)
   fun toString L = implode (map chr (rev L))
in
  fun gensym() = (state:=next (!state);toString (!state))
end;

datatype 'a cell 
  = NIL 
  | CELL of {data:'a, left:'a cell ref, right:'a cell ref};
type 'a dlist = 'a cell ref;

fun insertDlist a dlist =
    case dlist of
      ref (CELL{left=l1 as ref (CELL{right=r1,...}),...}) =>
      let val cell = CELL{data=a,
                          right=ref (!dlist),
                          left=ref (!l1)}
      in (dlist:=cell; l1:=cell; r1:=cell)
      end
    | ref NIL => 
      let val l = ref NIL
          val r = ref NIL
          val cell = CELL{data=a,left=l,right=r}
      in (dlist:=cell; l:=cell; r:=cell)
      end;
fun singletonDlist a =
    let val r = ref NIL
        val l = ref NIL
        val c = CELL{left=l, right=r, data=a}
    in  (r:=c;l:=c;r)
    end;


fun dataDlist (ref (CELL{data,...})) = data;
fun rightDlist (ref (CELL{right,...})) = right;
fun leftDlist (ref (CELL{left,...})) = left;
fun member a nil = false
  | member a (h::tl) = a = h orelse member a tl;

fun dlistToList L = 
    let fun f l visited = 
            if member l visited then nil
            else (dataDlist l)::(f (rightDlist l) (l::visited))
    in f (rightDlist (leftDlist L)) nil
    end;

(fn x => x = x) (ref 1);
ref 1 = ref 1;

ref (fn x => x);
val polyIdRef = ref (fn x => x);
polyIdRef := (fn x => x + 1);
(!polyIdRef) "You can't add one to me!";

val f = ((fn x => x) 1, fn x => fn y => (x + 1,ref y));
val g = (#2 f) 1;
val a = g "SML#"

val p = (1, fn x => x);
val id = #2 p;
id "SML#";

fun F x y = (x, y);
val G = F 1;
G "SML#";
EOF

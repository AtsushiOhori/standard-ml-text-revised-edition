smlsharp<<EOF
exception A;
fun f x = raise A;
fn x => (f 1 + 1, f "a" andalso true);

fun power m n = if m = 0 then 1
                else n * power (m - 1) n;
exception Undefined;
fun strictPower n m = if n = 0 andalso m = 0 then
                         raise Undefined
                      else power n m;
3 + strictPower 0 0;
3 + (strictPower 0 0 handle Undefined => 1);

datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;
type 'a dict = (string * 'a ) tree
exception NotFound
fun lookUp (key,Empty) = raise NotFound
  | lookUp (key,Node((key',v),L,R)) = 
    if key = key' then v
    else if key > key' then lookUp (key,R)
    else lookUp (key,L);
fun assoc (nil,dict) = nil
  | assoc ((h::t),dict) = 
    (h, lookUp (h,dict)):: assoc (t,dict)
    handle NotFound => (print "Undefined key.\n"; nil);

fun lookAll key dictList = 
    let exception Found of 'a
        fun lookUp key Empty = ()
          | lookUp key (Node((key',v),L,R)) = 
            if key = key' then raise Found v
            else if key > key' then lookUp key R
            else lookUp key L
    in (app (lookUp key) dictList; raise NotFound)
       handle Found v => v
    end;

fun makeMemoFun f = 
    let exception NotThere
        val memo = ref (fn x => (raise NotThere))
    in fn x => !memo x 
          handle NotThere => 
                 let val v = f x 
                     val oldMemo = !memo
                 in (memo := (fn y => if x = y then v 
                                      else oldMemo y);
                     v)
                 end
    end

local 
  exception NotThere 
  fun f memo 0 = 0
    | f memo 1 = 1
    | f memo n = !memo n
      handle NotThere =>
             let val v = f memo (n - 1) + f memo (n - 2)
                 val oldMemo = !memo
             in (memo := (fn y => if n = y then v 
                                  else oldMemo y);
                 v)
             end
in val fastFib = f (ref (fn x => raise NotThere))
end

EOF

smlsharp <<EOF
nil;
1::2::3::nil;
[[1],[1,2],[1,2,3]];
[fn x => x];
case nil of nil => 0 | (h::t) => h;
case [1,2] of nil => 0 | (h::t) => h;
fun length L = 
    case L of nil => 0
            | (h::t) => 1 + length t;
fun zip x = case x of (h1::t1,h2::t2) => (h1,h2) :: zip (t1,t2)
                    | _ => nil
fun unzip x = case x of (h1,h2)::t => 
                        let val (L1,L2) = unzip t 
                        in (h1::L1,h2::L2) 
                        end
                      | _ => (nil,nil);
fun last L = case L of [x] => x
                     | (h::t) => last t;
fun length nil = 0
  | length (h::t) = 1 + length t;

map (fn x => (x,x)) [1,2,3,4];
fun length l = if null l then 0
               else 1 + length (tl l);
fun foldr f Z nil = Z
  | foldr f Z (h::t) = f(h,foldr f Z t);
val sumList = foldr (fn (h,R) => h + R) 0 ;
fun length L = foldr (fn (_,R)=>1 + R) 0 L;
infixr 5 @;
fun L1 @ L2 = foldr (op ::) L2 L1;

fun timesRel (R,S) = 
    foldr (fn ((x,a),r) => 
              foldr (fn ((b,y),rs) => 
                        if a=b then (x,y)::rs else rs) 
                    r S)
          nil R;
fun powerRel r 1 = r
  | powerRel r n = timesRel (r,powerRel r (n - 1));

fun accumulate h z f n = 
    if n = 0 then z
    else h(f n,accumulate h z f (n - 1));
fun tc R = accumulate (op @) nil (powerRel R) (length R);
tc [(1,2),(2,3),(3,4)];
EOF

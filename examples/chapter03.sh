smlsharp<<EOF
fun products f n = if n = 0 then 1
             else f n * products f (n - 1);
fun factorial n = products n (fn x => x);
fun id x = x;
id 21;
id "Standard ML";
id products;
fn x => id id x;
fun twice f x = f (f x);
fun power m n = if m = 0 then 1
                else n * power (m - 1) n;
power 3 2;
val cube = power 3;
twice cube 2;
twice (fn x => x ^ x) "ML";
fn x => twice twice x;
it (fn x => x + 1) 1;
fun intTwice f (x:int) = f (f x);
fun intTwice (f:int -> int) x = f (f x);
fun intTwice f x = f (f x) : int;
fun intTwice f x : int = f (f x);
fun higherTwice f (x:'a -> 'a) = f (f x);
1 + 1;
fun f x = x + 1.0;
fun f x y = x > x;
fun f x y = x > (abs y);
val fourTimes = twice twice;
val fourTimes = fn f => twice twice f;
op =;
fun memo f x = let val a = f x
               in fn y => if x = y then a else f y 
               end;
EOF

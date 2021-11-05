smlsharp <<EOF
fun x(n,a,b,c,d) = if n = 0 then 1
                    else a * x(n-1,a,b,c,d) + b * z(n-1,a,b,c,d)
and y(n,a,b,c,d) = if n = 0 then 0
                   else a * y(n-1,a,b,c,d) + b * w(n-1,a,b,c,d)
and z(n,a,b,c,d) = if n = 0 then 0
                   else c * x(n-1,a,b,c,d) + d * z(n-1,a,b,c,d)
and w(n,a,b,c,d) = if n = 0 then 1
                   else c * y(n-1,a,b,c,d) + d * w(n-1,a,b,c,d);
fun matrixPower (n,a,b,c,d) = 
    if n = 0 then (1,0,0,1)
    else let val (x,y,z,w) = matrixPower(n-1,a,b,c,d) 
         in  (a*x+b*z,a*y+b*w,c*x+d*z,c*y+d*w)
         end;
val a = matrixPower(20, 1,1,1,1);
fun Power(m,n) = if m = 0 then 1
                 else n * Power(m - 1,n);
Power(3,2);
fun power m n = if m = 0 then 1
                else n * power (m - 1) n;
power 3 2;
val cube = power 3;
cube 2;
fun sumOfCube n = if n = 1 then cube 1
                  else cube n + sumOfCube (n - 1);
sumOfCube 3;
fun summation f n = if n = 1 then f 1
                    else f n + summation f (n - 1);
val newSumOfCube = summation cube;
newSumOfCube 3;
val sumOfSquare = summation (power 2);
sumOfSquare 3;
summation (power 4) 3;
fn x => x + 1;
(fn x => x + 1) 3  * 10;
fun fib n = if n = 0 then 0
            else if n = 1 then 1
            else fib (n - 1) + fib (n - 2);
fun f n m = (fib n) mod m = 0;
val g = f 35;
(g 1,g 2,g 3,g 4,g 5);
fun f n = let val a = (fib n)
          in fn m => a mod m = 0
          end;
val g = f 35;
(g 1,g 2,g 3,g 4,g 5);
fun memo f x = let val a = f x
               in fn y => if x = y then a else f y
               end;
val fib = memo fib 31;
val fib = memo fib 32;
val fib = memo fib 33;
val fib = memo fib 34;
val x = 10;
val y = x * 2;
val x = 20;
y;
val x = 10;
val y = 20;
fun f x = x + y;
f 3;
val y = 99;
f 3;
infix 8 Power;
2 Power 3 + 10;
Power;
op Power;
op Power (2,3);
infix 9 hyperexp;
fun op hyperexp (x,y) =
    if x = 0 then y
    else 2 Power (x - 1) hyperexp y;
2 hyperexp 2;
2 Power 2 hyperexp 2;
(2 Power 2) hyperexp 2;
(fn f => fn x => (f(x,x))) op hyperexp 2;
3 Power 2 Power 2;
3 Power (2 Power 2);
infixr 8 Power;
3 Power 2 Power 2;
(3 Power 2) Power 2;
nonfix Power;
Power (2,3);
EOF

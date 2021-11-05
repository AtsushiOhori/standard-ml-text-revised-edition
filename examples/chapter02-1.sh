smlsharp <<EOF
fun double x = x * 2;
double 2;
double 3 + 1;
double (double 2) + 3;
fun f (x,y) = x * 2 + y;
f (2,3);
("Oleo",("Kenny","Drew"),1975);
fun factorial n = if n = 0 then 1
                  else n * (factorial (n - 1));
factorial 6;
fun factorial n =
    let fun fact n a = if n = 0 then a
                       else fact (n - 1) (n * a)
    in fact n 1
    end;
fact 6 1;
let
   val pi = 3.141592
   fun f r = 2.0 * pi * r
in
   f 10.0
end * 10.0;
local
   fun fact n a = if n = 0 then a
                 else fact (n - 1) (n*a)
in
   fun factorial n = fact n 1
end;
fact;
fun F x = if x > 2000.0 then 0.02 
          else 0.01 + 0.01 * (x  - 1000.0) / 1000.0;
fun I(x,n) = F(A(x,n - 1))
and A(x,n) = if n = 0 then x
             else A(x,n-1)*(1.0+I(x,n));
A(100.0, 10);
A(100.0, 20);
A(100.0, 25);
A(100.0, 30);
EOF


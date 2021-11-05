smlsharp<<EOF
21;
3.14;
1E2;
true;
false;
#"A";
"Hi, there!";
22 - (23 mod 3);
3.14 * 6.0 * 6.0 * (60.0/360.0);
2 * 4 div (5 - 3)
* 3 + (10 - 7) + 6;
(7 mod 2) = 1;
(if (7 mod 2) = 0 then 7 else 7 - 1) * 10;
31;
it;
it + 1;
it;
if #"A" > #"a" then ord #"A" else ord #"a";
chr 97;
str it;
"SML" > "Lisp";
"Standard " ^ "ML";
    if #"?" < #"$" then
       if #"$" < #"*" then
         (str #"?") ^ (str #"$") ^ (str #"*")
       else if #"?" < #"*" then
         (str #"?") ^ (str #"*") ^ (str #"$") 
       else (str #"*") ^ (str #"?") ^ (str #"$") 
    else if #"?" < #"*" then 
      (str #"$") ^ (str #"?") ^ (str #"*") 
    else if #"$" < #"*" then 
      (str #"$") ^ (str #"*") ^ (str #"?") 
    else (str #"*") ^ (str #"$") ^ (str #"?") ;
(2 + 2] + 4);
33 + "cat";
10 * 3.14;
real 10;
it * 3.14;
val OneMile = 1.6093;
OneMile;
100.0 / OneMile;
val OneMile = 1.609;
val OneMile = 1609;
OneMile * 55;
onemile * 55;
EOF

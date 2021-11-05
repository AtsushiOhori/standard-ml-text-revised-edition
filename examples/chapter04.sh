smlsharp <<EOF
1 before print "One\n";
ignore 3;
123;
~0x10;
0w123;
0wx10;
0w123 : word64;
100000000000000000000 : intInf;
val a = 1.1 / 0.0;
a * ~1.0;
a / it;
"This is a single \ 
\string constant.";
"Standard ML";
substring(it,9,2);
explode it;
map (fn x => ord x + 1) it;
map chr it;
implode it;
it < "ML";
EOF

smlsharp mySmlsharp.sml
./a.out
smlsharp -c InfList.sml
smlsharp -c Prime.sml
smlsharp -c Main.sml
smlsharp Main.smi
./Main
smlsharp -MMm Main.smi > Makefile
make
./Main
smlsharp<<EOF
val rand = _import "rand" : () -> int;
val srand = _import "rand" : word -> ();
srand(0wx10);
rand();
EOF



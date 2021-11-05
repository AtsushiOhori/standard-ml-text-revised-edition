SMLSHARP = smlsharp
SMLFLAGS = -O2
LIBS =
all: Main
Main: InfList.smi Prime.smi Main.smi InfList.o Prime.o Main.o
	$(SMLSHARP) $(LDFLAGS) -o Main Main.smi $(LIBS)
InfList.o: InfList.sml InfList.smi
	$(SMLSHARP) $(SMLFLAGS) -o InfList.o -c InfList.sml
Prime.o: Prime.sml InfList.smi Prime.smi
	$(SMLSHARP) $(SMLFLAGS) -o Prime.o -c Prime.sml
Main.o: Main.sml InfList.smi Prime.smi Main.smi
	$(SMLSHARP) $(SMLFLAGS) -o Main.o -c Main.sml

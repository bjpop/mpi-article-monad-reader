all: Trapezoid Trapezoid-multicore Trapezoid-mpi Trapezoid-c 

%: %.hs
	ghc --make -O2 -threaded -rtsopts $<

%: %.c
	gcc -std=c99  -o $@ $< -lmpi -I/usr/include/mpi

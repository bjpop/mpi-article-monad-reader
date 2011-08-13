all: single-threaded multi-threaded mpi

%: %.hs Trapezoid.hs
	ghc --make -O2 -threaded -rtsopts $<

clean:
	rm *.hi *.o single-threaded multi-threaded mpi

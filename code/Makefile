all: single-threaded multi-threaded mpi-p2p mpi-gather

%: %.hs Trapezoid.hs
	ghc --make -O2 -threaded -rtsopts $<

clean:
	rm *.hi *.o single-threaded multi-threaded mpi-p2p mpi-gather

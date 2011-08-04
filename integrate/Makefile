all: plain threaded mpi compare.txt diffs

plain: plain.hs
	ghc --make -O2 -rtsopts $<

threaded: threaded.hs
	ghc --make -O2 -threaded -rtsopts $<

mpi: mpi.hs
	ghc --make -O2 -threaded -rtsopts $<

compare.txt: plain threaded mpi
	/usr/bin/time -o $@                      ./plain    +RTS -H50M    > plain.txt
	/usr/bin/time -o $@ --append             ./threaded +RTS -H50M -N > threaded.txt
	/usr/bin/time -o $@ --append mpirun -n 4 ./mpi      +RTS -H50M -N > mpi.txt
	cat compare.txt

diffs: plain-to-threaded.patch threaded-to-mpi.patch

plain-to-threaded.patch: plain.hs threaded.hs
	diff -u plain.hs threaded.hs > $@ || true

threaded-to-mpi.patch: threaded.hs mpi.hs
	diff -u threaded.hs mpi.hs > $@ || true

clean:
	rm plain threaded mpi *.o *.hi *.patch *.txt

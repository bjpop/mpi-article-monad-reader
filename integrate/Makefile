all: plain threaded compare.txt diffs

plain: plain.hs
	ghc --make -O2 -rtsopts $<

threaded: threaded.hs
	ghc --make -O2 -threaded -rtsopts $<

compare.txt: plain threaded
	/usr/bin/time -o $@          ./plain    +RTS -H50M    > /dev/null
	/usr/bin/time -o $@ --append ./threaded +RTS -H50M -N > /dev/null
	cat compare.txt

diffs: plain-to-threaded.patch

plain-to-threaded.patch: plain.hs threaded.hs
	diff -u plain.hs threaded.hs > plain-to-threaded.patch || true

clean:
	rm plain threaded *.o *.hi *.patch compare.txt

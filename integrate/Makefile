all: plain threaded diffs

plain: plain.hs
	ghc --make -O2 $<

threaded: threaded.hs
	ghc --make -O2 -threaded -rtsopts $<

diffs: plain-to-threaded.patch

plain-to-threaded.patch: plain.hs threaded.hs
	diff -u plain.hs threaded.hs > plain-to-threaded.patch || true

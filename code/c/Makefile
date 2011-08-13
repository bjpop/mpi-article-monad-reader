all: mpi

%: %.c
	gcc -std=c99  -o $@ $< -lmpi -I/usr/include/mpi

clean:
	rm *.o mpi

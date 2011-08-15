set term postscript eps enhanced color

set title "Performance results.\n Integration of Sine on the interval [0,1000{/Symbol p}], with 10^9 trapezoids."
set logscale xy 2
set xlabel "CPU cores"
set ylabel "Time (seconds)"
set key at 128,32

plot 'performance.dat' using 1:2 with linespoints title "Haskell-sequential", \
     'performance.dat' using 1:3 with linespoints title "Haskell-threads", \
     'performance.dat' using 1:4 with linespoints title "Haskell-MPI", \
     'performance.dat' using 1:5 with linespoints title "C-MPI"

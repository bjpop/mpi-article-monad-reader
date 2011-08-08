#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

double f(double);
double trapezoid(double, double, int, double);

int main(int argc, char **argv) {

   double a, b, h, local_a, local_b, integral, sum = 0;
   double *results = NULL;
   int n, local_n, rank, num_ranks;

   MPI_Init(&argc, &argv);
   MPI_Comm_size(MPI_COMM_WORLD, &num_ranks);
   MPI_Comm_rank(MPI_COMM_WORLD, &rank);

   a = atof(argv[1]);
   b = atof(argv[2]);
   n = atoi(argv[3]);

   h = (b - a) / n;
   local_n = n / num_ranks;
   local_a = a + rank * local_n * h;
   local_b = local_a + local_n * h;
   integral = trapezoid(local_a, local_b, local_n, h);

   if (rank == 0)
      results = (double *) malloc(num_ranks * sizeof(double));

   MPI_Gather(&integral, 1, MPI_DOUBLE, results, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD);

   if (rank == 0) {

      for (int i = 0; i < num_ranks; i++)
         sum += results[i];

      printf("%lf\n", sum);
   }

   MPI_Finalize();
}

double trapezoid(double a, double b, int n, double h) {

   double result;
   double x = a;
   result = (f(a) + f(b)) / 2.0;
   for (int i = 1; i < n; i++) {
      x = x + h;
      result += f(x);
   }
   return result * h;
}

double f(double x) {

   //return 1;
   return 4.0 / (1 + x * x);
}

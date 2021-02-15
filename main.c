#include "reduction.h"
#include <stdlib.h>
#include <stdio.h>


void print_matrix(int64_t* matrix, int64_t n) {
  for (int64_t j = 0; j < n; j++) {
    for (int64_t i = 0; i < n; i++) {
      printf("%ld ", matrix[i * n + j]);
    }
    printf("\n");
  }
}

void print_array(int64_t* array, int64_t n) {
  for (int64_t i = 0; i < n; i++) {
    printf("%ld ", array[i]);
  }
  printf("\n");
}

int main(int argc, char **argv) {

  int64_t n = 7;
  int64_t matrix[] = {
    0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,
    1,1,0,0,0,0,0,
    1,0,1,0,0,0,0,
    0,1,1,0,0,0,0,
    0,0,0,1,1,1,0
  };

  printf("Initial matrix is:\n");
  print_matrix(matrix, n);

  struct futhark_context_config* config = futhark_context_config_new();
  struct futhark_context* context = futhark_context_new(config); 

  const char* error = futhark_context_get_error(context);
  if (error != NULL) {
    printf("Failed to create Futhark context.\n");
    printf("%s", error);
    exit(EXIT_FAILURE);
  }

  printf("Building matrix on host...\n");
  struct futhark_i64_2d* fut_matrix =
    futhark_new_i64_2d(context, matrix, n, n);

  printf("Reducing...\n");
  struct futhark_i64_2d* fut_reduced_matrix = NULL;
  struct futhark_i64_1d* fut_reduced_lows = NULL;
  futhark_entry_reduce_matrix(context, &fut_reduced_matrix, &fut_reduced_lows, fut_matrix);

  printf("Copying to host...\n");
  int64_t* reduced_matrix = calloc(n * n, sizeof(int64_t));
  int64_t* reduced_lows = calloc(n, sizeof(int64_t));
  futhark_values_i64_2d(context, fut_reduced_matrix, reduced_matrix);
  futhark_values_i64_1d(context, fut_reduced_lows, reduced_lows);

  futhark_context_sync(context);

  printf("Done\n");

  printf("Reduced matrix is:\n");
  print_matrix(reduced_matrix, n);
  free(reduced_matrix);

  printf("Reduced lows are:\n");
  print_array(reduced_lows, n);
  free(reduced_lows);

  printf("Freeing futhark data\n");
  futhark_free_i64_2d(context, fut_matrix);
  futhark_free_i65_1d(context, fut_reduced_lows);
  futhark_free_i64_2d(context, fut_reduced_matrix);

  printf("Freeing futhark context and config\n");
  futhark_context_free(context);
  futhark_context_config_free(config);
}


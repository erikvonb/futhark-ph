#include "reduction.h"
#include "io.c"
#include <stdlib.h>
#include <stdio.h>


void print_matrix(int32_t* matrix, int n) {
  for (size_t j = 0; j < n; j++) {
    for (size_t i = 0; i < n; i++) {
      int32_t v = matrix[i * n + j];
      if (v == 0) {
        printf(". ");
      } else {
        printf("%d ", v);
      }
    }
    printf("\n");
  }
}

void print_array(int64_t* array, int n) {
  for (size_t i = 0; i < n; i++) {
    printf("%ld ", array[i]);
  }
  printf("\n");
}

void reduce(int32_t* matrix, int64_t n, int32_t* out_matrix, int64_t* out_lows) {

  struct futhark_context_config* config = futhark_context_config_new();
  struct futhark_context* context = futhark_context_new(config); 

  const char* error = futhark_context_get_error(context);
  if (error != NULL) {
    printf("Failed to create Futhark context.\n");
    printf("%s", error);
    exit(EXIT_FAILURE);
  }

  printf("Building matrix on host...\n");
  struct futhark_i32_2d* fut_matrix =
    futhark_new_i32_2d(context, matrix, n, n);

  printf("Reducing...\n");
  struct futhark_i32_2d* fut_reduced_matrix = NULL;
  struct futhark_i64_1d* fut_reduced_lows = NULL;
  futhark_entry_reduce_matrix(context, &fut_reduced_matrix, &fut_reduced_lows, fut_matrix);

  printf("Copying to host...\n");
  futhark_values_i32_2d(context, fut_reduced_matrix, out_matrix);
  futhark_values_i64_1d(context, fut_reduced_lows, out_lows);

  futhark_context_sync(context);

  printf("Done\n");
  printf("Freeing futhark data\n");
  futhark_free_i32_2d(context, fut_matrix);
  futhark_free_i64_1d(context, fut_reduced_lows);
  futhark_free_i32_2d(context, fut_reduced_matrix);

  printf("Freeing futhark context and config\n");
  futhark_context_free(context);
  futhark_context_config_free(config);
}

int main(int argc, char *argv[]) {
  if (argc != 3) {
    printf("Wrong number of input arguments.\n");
    return  EXIT_FAILURE;
  }

  char* input_file = argv[1];
  char* output_file = argv[2];

  int n;
  int32_t* matrix = malloc(n * n * sizeof(int32_t));
  read_dense_matrix(input_file, matrix, &n);

  /*int* sparse_rows = malloc(n * sizeof(int));*/
  /*int* sparse_cols = malloc(n * sizeof(int));*/
  /*read_sparse_matrix(sparse_rows, sparse_cols, n,*/
      /*"../datasets/sparse/klein_bottle_pointcloud_new_400.txt");*/
      /*"../datasets/sparse/test.txt");*/

  printf("Initial matrix is:\n");
  print_matrix(matrix, n);

  int32_t* reduced_matrix = calloc(n * n, sizeof(int32_t));
  int64_t* reduced_lows = calloc(n, sizeof(int64_t));
  reduce(matrix, n, reduced_matrix, reduced_lows);

  printf("Reduced matrix is:\n");
  print_matrix(reduced_matrix, n);
  printf("Writing matrix to file\n");
  write_dense_matrix(reduced_matrix, n, output_file);
  free(reduced_matrix);

  printf("Reduced lows are:\n");
  print_array(reduced_lows, n);
  free(reduced_lows);
}


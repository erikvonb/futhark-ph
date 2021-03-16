#include "reduction.h"
#include "io.c"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>


int32_t* sparse_to_dense(int32_t* col_idxs, int32_t* row_idxs, int nnz, int n) {
  int32_t* matrix = calloc(n * n, sizeof(int32_t));
  for (size_t k = 0; k < nnz; k++) {
    int j = col_idxs[k];
    int i = row_idxs[k];
    matrix[i * n + j] = 1;
  }

  return matrix;
}

void print_matrix(int32_t* matrix, int n) {
  for (size_t i = 0; i < n; i++) {
    for (size_t j = 0; j < n; j++) {
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

void print_array_32(int32_t* array, int n) {
  for (size_t i = 0; i < n; i++) {
    printf("%d ", array[i]);
  }
  printf("\n");
}

void reduce(
    int32_t* col_idxs, int32_t* row_idxs, int64_t n, int nnz,
    bool should_write_matrix,
    int32_t** out_col_idxs, int32_t** out_row_idxs, int64_t** out_lows,
    int64_t* out_nnz) {

  struct futhark_context_config* config = futhark_context_config_new();
  struct futhark_context* context = futhark_context_new(config); 

  const char* error = futhark_context_get_error(context);
  if (error != NULL) {
    printf("Failed to create Futhark context.\n");
    printf("%s", error);
    exit(EXIT_FAILURE);
  }

  printf("Copying data to device...\n");
  struct futhark_i32_1d* fut_col_idxs =
    futhark_new_i32_1d(context, col_idxs, nnz);
  struct futhark_i32_1d* fut_row_idxs =
    futhark_new_i32_1d(context, row_idxs, nnz);

  printf("Reducing...\n");
  struct futhark_i32_1d* fut_reduced_col_idxs = NULL;
  struct futhark_i32_1d* fut_reduced_row_idxs = NULL;
  struct futhark_i64_1d* fut_reduced_lows = NULL;
  futhark_entry_reduce_matrix(
      context,
      &fut_reduced_col_idxs,
      &fut_reduced_row_idxs,
      &fut_reduced_lows,
      fut_col_idxs,
      fut_row_idxs,
      n);

  printf("Done reducing\n");
  futhark_context_sync(context);

  const int64_t* reduced_nnz_dims =
    futhark_shape_i32_1d(context, fut_reduced_col_idxs);

  int32_t* reduced_col_idxs = malloc(reduced_nnz_dims[0] * sizeof(int32_t));
  int32_t* reduced_row_idxs = malloc(reduced_nnz_dims[0] * sizeof(int32_t));
  int64_t* reduced_lows = malloc(n * sizeof(int64_t));

  printf("Copying to host...\n");
  if (should_write_matrix) {
    futhark_values_i32_1d(context, fut_reduced_col_idxs, reduced_col_idxs);
    futhark_values_i32_1d(context, fut_reduced_row_idxs, reduced_row_idxs);

  } else {
    futhark_values_i64_1d(context, fut_reduced_lows, reduced_lows);
  }

  printf("Done\n");
  printf("Freeing futhark data\n");
  futhark_free_i32_1d(context, fut_col_idxs);
  futhark_free_i32_1d(context, fut_row_idxs);
  futhark_free_i32_1d(context, fut_reduced_col_idxs);
  futhark_free_i32_1d(context, fut_reduced_row_idxs);
  futhark_free_i64_1d(context, fut_reduced_lows);

  printf("Freeing futhark context and config\n");
  futhark_context_free(context);
  futhark_context_config_free(config);

  *out_nnz = reduced_nnz_dims[0];
  *out_col_idxs = reduced_col_idxs;
  *out_row_idxs = reduced_row_idxs;
  *out_lows = reduced_lows;
}

int main(int argc, char *argv[]) {
  char* input_file;
  char* output_file;
  bool should_write_matrix = false;

  char c;
  while( (c = getopt(argc, argv, "i:o:m")) != -1 ) {
    switch(c) {
      case 'i':
        input_file = optarg;
        break;
      case 'o':
        output_file = optarg;
        break;
      case 'm':
        should_write_matrix = true;
        break;
    }
  }

  int64_t n;
  int32_t nnz;
  int32_t* row_idxs;
  int32_t* col_idxs;
  read_sparse_matrix(&row_idxs, &col_idxs, &nnz, &n, input_file);

  /*printf("Initial matrix is:\n");*/
  /*print_matrix(sparse_to_dense(col_idxs, row_idxs, nnz, n), n);*/

  int64_t reduced_nnz;
  int32_t* reduced_col_idxs;
  int32_t* reduced_row_idxs;
  int64_t* reduced_lows;

  reduce(col_idxs, row_idxs, n, nnz, should_write_matrix,
      &reduced_col_idxs, &reduced_row_idxs, &reduced_lows, &reduced_nnz);

  free(col_idxs);
  free(row_idxs);

  if (should_write_matrix) {
    int32_t* reduced_dense_matrix =
      sparse_to_dense(reduced_col_idxs, reduced_row_idxs, reduced_nnz, n);

    /*printf("Reduced matrix is:\n");*/
    /*print_matrix(reduced_dense_matrix, n);*/
    printf("Writing matrix to file\n");
    write_dense_matrix(reduced_dense_matrix, n, output_file);
    
    free(reduced_dense_matrix);
    free(reduced_col_idxs);
    free(reduced_row_idxs);

  } else {
    /*printf("Reduced lows are:\n");*/
    /*print_array(reduced_lows, n);*/
    printf("Writing lows to file\n");
    write_array(reduced_lows, n, output_file);
    free(reduced_lows);
  }
}


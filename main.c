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

bool handle_error(struct futhark_context *ctx, int e, char* entry_name) {
  if (e != 0) {
    char* error_string = futhark_context_get_error(ctx);
    printf("Entry %s failed with error:\n%s\n", entry_name, error_string);
    free(error_string);
    return true;
  }
  return false;
}

int reduce(
    int32_t* col_idxs, int32_t* row_idxs, int64_t n, int nnz,
    bool should_write_matrix, bool debug,
    int32_t** out_col_idxs, int32_t** out_row_idxs, int64_t** out_lows,
    int64_t* out_nnz) {

  struct futhark_context_config* config = futhark_context_config_new();
  if (debug) {
    futhark_context_config_set_debugging(config, 1);
  }
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
  int err;
  struct futhark_opaque_state* fut_state_in;
  struct futhark_opaque_state* fut_state_out;
  err = futhark_entry_init_state(context, &fut_state_in,
      fut_col_idxs, fut_row_idxs, n);
  if (handle_error(context, err, "init_state")) {
    return 1;
  }

  bool converged = false;
  while (!converged) {
    err = futhark_entry_iterate_step(context, &fut_state_out, fut_state_in);
    if (handle_error(context, err, "iterate_step")) {
      return 1;
    }
    futhark_entry_is_reduced(context, &converged, fut_state_out);

    struct futhark_opaque_state* tmp = fut_state_in;
    fut_state_in = fut_state_out;
    fut_state_out = tmp;
  }

  printf("Done reducing\n");
  futhark_context_sync(context);

  int64_t reduced_nnz;
  err = futhark_entry_state_nnz(context, &reduced_nnz, fut_state_in);
  if (handle_error(context, err, "state_nnz")) {
    return 1;
  }
  futhark_context_sync(context);
  printf("Final number of nonzeroes is %ld\n", reduced_nnz);

  struct futhark_i32_1d* fut_reduced_col_idxs;
  struct futhark_i32_1d* fut_reduced_row_idxs;
  struct futhark_i64_1d* fut_reduced_lows;
  int32_t* reduced_col_idxs = malloc(reduced_nnz * sizeof(int32_t));
  int32_t* reduced_row_idxs = malloc(reduced_nnz * sizeof(int32_t));
  int64_t* reduced_lows = malloc(n * sizeof(int64_t));

  printf("Copying to host...\n");
  futhark_entry_state_contents(context,
      &fut_reduced_col_idxs, &fut_reduced_row_idxs, &fut_reduced_lows,
      fut_state_in);
  futhark_context_sync(context);

  if (should_write_matrix) {
    futhark_values_i32_1d(context, fut_reduced_col_idxs, reduced_col_idxs);
    futhark_values_i32_1d(context, fut_reduced_row_idxs, reduced_row_idxs);

  } else {
    futhark_values_i64_1d(context, fut_reduced_lows, reduced_lows);
  }

  printf("Freeing futhark data\n");
  futhark_free_opaque_state(context, fut_state_in);
  futhark_free_opaque_state(context, fut_state_out);
  futhark_free_i32_1d(context, fut_col_idxs);
  futhark_free_i32_1d(context, fut_row_idxs);
  futhark_free_i32_1d(context, fut_reduced_col_idxs);
  futhark_free_i32_1d(context, fut_reduced_row_idxs);
  futhark_free_i64_1d(context, fut_reduced_lows);

  printf("Freeing futhark context and config\n");
  futhark_context_free(context);
  futhark_context_config_free(config);

  *out_nnz = reduced_nnz;
  *out_col_idxs = reduced_col_idxs;
  *out_row_idxs = reduced_row_idxs;
  *out_lows = reduced_lows;

  return 0;
}

int main(int argc, char *argv[]) {
  char* input_file;
  char* output_file;
  bool should_write_matrix = false;
  bool debug = false;

  char c;
  while( (c = getopt(argc, argv, "i:o:md")) != -1 ) {
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
      case 'd':
        debug = true;
        break;
    }
  }

  if (input_file == NULL) {
    printf("No input file given. Try -i\n");
    return EXIT_FAILURE;
  }
  if (output_file == NULL) {
    printf("No output file given. Try -o\n");
    return EXIT_FAILURE;
  }

  int64_t n;
  int32_t nnz;
  int32_t* row_idxs;
  int32_t* col_idxs;
  read_sparse_matrix(&row_idxs, &col_idxs, &nnz, &n, input_file);

  printf("Initial matrix has %ld columns and %d nonzeroes\n", n, nnz);
  /*printf("Initial matrix is:\n");*/
  /*print_matrix(sparse_to_dense(col_idxs, row_idxs, nnz, n), n);*/

  int64_t reduced_nnz;
  int32_t* reduced_col_idxs;
  int32_t* reduced_row_idxs;
  int64_t* reduced_lows;

  int e = reduce(col_idxs, row_idxs, n, nnz, should_write_matrix, debug,
      &reduced_col_idxs, &reduced_row_idxs, &reduced_lows, &reduced_nnz);

  free(col_idxs);
  free(row_idxs);

  if (e != 0) {
    printf("Reduction failed.\n");
    return EXIT_FAILURE;
  }

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


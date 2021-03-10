#include <stdlib.h>
#include <stdio.h>


int count_lines(char* filename) {
  FILE* fp = fopen(filename, "r");
  if (!fp) {
    printf("Filename for matrix invalid\n");
  }
  
  int n_lines = 0;
  while (!feof(fp)) {
    char c = fgetc(fp);
    if (c == '\n') {
      n_lines++;
    }
  }

  fclose(fp);
  return n_lines;
}

void read_sparse_matrix(
    int32_t** out_rows, int32_t** out_cols, int32_t* out_nnz, int64_t* out_n,
    char* filename) {
  int nnz = count_lines(filename);
  
  int n = 0;
  int32_t* rows = malloc(nnz * sizeof(int));
  int32_t* cols = malloc(nnz * sizeof(int));

  FILE* fp = fopen(filename, "r");
  if (!fp) {
    printf("Filename for dataset invalid\n");
  }

  for (size_t i = 0; i < nnz; i++) {
    int r, c;
    int num_success = fscanf(fp, "%d %d\n", &r, &c);
    if (num_success != 2) {
      printf("Failed to read data at row %ld\n", i);
      exit(EXIT_FAILURE);
    }
    rows[i] = r;
    cols[i] = c;
    n = c + 1 > n ? c + 1 : n;
  }

  *out_n = n;
  *out_nnz = nnz;
  *out_rows = rows;
  *out_cols = cols;
  fclose(fp);
}

void read_dense_matrix(char* filename, int32_t* out_matrix, int* out_n) {
  int n = count_lines(filename);

  FILE* fp = fopen(filename, "r");

  for (size_t i = 0; i < n; i++) {
    for (size_t j = 0; j < n; j++) {
      int v;
      int num_success = fscanf(fp, "%d%*[ \n]", &v);
      /*printf("(ns was %d) ", num_success);*/
      /*if (num_success != 1) {*/
        /*printf("Failed to read data at (%ld, %ld)\n", i, j);*/
      /*}*/
      out_matrix[j * n + i] = v;
      /*printf("%d", v);*/
    }
    /*printf("\n");*/
  }
  *out_n = n;
  fclose(fp);
}

void write_dense_matrix(int32_t* matrix, int n, char* filename) {
  FILE* fp = fopen(filename, "w");

  for (size_t i = 0; i < n; i++) {
    for (size_t j = 0; j < n; j++) {
      int v = matrix[j * n + i];
      char c = v == 0 ? '0' : '1';
      fputc(c, fp);

      if (j < n - 1) {
        fputc(' ', fp);
      }
    }
    fputc('\n', fp);
  }
  fclose(fp);
}


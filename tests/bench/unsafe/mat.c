//test return 859104960
#include <stdbool.h>
#include <stdlib.h>
extern void _c0_assert(bool param);
// -O0: 5.199s
// -O1: 4.139s
// -O1 -foptimize-sibling-calls: 4.085s

int** create_matrix(int size) {
  int** M = calloc(size, sizeof(int*));
  int count = 0;
  for (int i = 0; i < size; i++) {
    M[i] = calloc(size, sizeof(int));
    count = i;
  }
  return M;
}

int fill_matrix(int** M, int size, int val) {
  for (int c = 0; c < size; c++) {
    for (int r = 0; r < size; r++) {
      M[r][c] = val;
    }
  }
  return val;
}

// add each column to the next column
void roll_col(int** M, int size, int start_col) {
  if (start_col < 0 || start_col >= size) return;
  for (int r = 0; r < size; r++) {
    if (start_col == 0) {
      M[r][start_col] *= 1;
    } else {
      M[r][start_col] += M[r][start_col-1];
    }
  }
  roll_col(M, size, start_col+1);
}

void roll_row(int** M, int size, int start_row) {
  if (start_row < 0 || start_row >= size) return;
  for (int c = 0; c < size; c++) {
    if (start_row == 0) {
      M[start_row][c] *= 1;
    } else {
      M[start_row][c] += M[start_row-1][c];
    }
  }
  roll_row(M, size, start_row+1);
}

// square matrix multiply
void matrix_mult(int** C, int** A, int** B, int size) {
  int sum = 0;

  for (int r = 0; r < size; r++) {
    for (int c = 0; c < size; c++) {
      for (int i = 0; i < size; i++) {
        C[r][c] += A[r][i] * B[i][c];
      }
    }
  }
}

// harness
struct data {
  int n;
  int** A;
  int** B;
  int** C;
};
typedef struct data data_t;

data_t *_c0_init(int param) {
  data_t *result = calloc(1, sizeof(data_t));
  result->n = 500;
  result->A = create_matrix(result->n);
  result->B = create_matrix(result->n);
  result->C = create_matrix(result->n);
  return result;
}
void _c0_prepare(data_t *result, int param) {}
void _c0_run(data_t *result, int param) {
  fill_matrix(result->A, result->n, 1);
  fill_matrix(result->B, result->n, 2);

  roll_col(result->A, result->n, 0); roll_row(result->A, result->n, 0);
  roll_row(result->B, result->n, 0); roll_col(result->B, result->n, 0);
  
  matrix_mult(result->C, result->A, result->B, result->n);
}
int _c0_checksum(data_t *result, int param) {
  return (result->C)[result->n-1][result->n-1];
}

int _c0_main() {
  data_t *result = _c0_init(0);
  _c0_prepare(result, 0);
  _c0_run(result, 0);
  return _c0_checksum(result, 0);
}
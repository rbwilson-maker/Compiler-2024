//test return -15875854

// 0.671 on gcc -O1 and 1.764 on gcc -O0 

#include <stdbool.h>
#include <stdlib.h>
extern void _c0_assert(bool param);

int _c0_rand(int seed) {
  return 1103515245 * seed + 12345;
}

int _c0_mod(int v, int count) {
  int ret = v % count;
  if (ret < 0) {
    ret += count;
  }
  return ret;
}


int _c0_f(int x, int y) {
  int n = 32509987;

  int rows = 30000;
  int cols = 1000;
  int* a = calloc(rows * cols, sizeof(int));

  for (int j = 0; j < cols; j++) {
    for (int i = 0; i < rows; i++) {
      a[i * cols + j] = rows * cols - (i * cols);
    }
  }

  int y_hidden = y + a[500];
  for (int z = 0; z < 3; z++) {
    for (int j = 1; j < cols; j++) {
      for (int i = j; i < rows; i++) {
        int factor_1 = x * y_hidden;
        int factor_2 = x * (y_hidden / 7);
        int factor_3 = x * ((y_hidden / 7) * 3);
        int factor_4 = x * (((y_hidden / 7) * 3) * 6);
        int factor_5 = x * ((((y_hidden / 7) * 3) * 6) / y_hidden);
        a[i*cols + j] = a[i * cols + j - 1] * ((factor_1 * factor_2 * factor_3 * factor_4 * factor_5) / (rows * cols / 50));
      }
    }
    a[0] = a[cols * rows - 1];
    for (int i = 0; i < 5000; i++) {
      int n1 = _c0_rand(n);
      n = _c0_rand(n1);
      a[_c0_mod(n1, rows * cols)] = n;
    }
  }
  int current = 0;
  for (int j = 0; j < cols; j++) {
    for (int i = 0; i < rows; i++) {
      current += a[i * cols + j];
    }
  }
  return current;
}

int* _c0_init(int _) { return calloc(sizeof(int), 1); }
void _c0_prepare(int* dat, int _) { *dat = 0; }
int _c0_checksum(int* p) { return *p; }

void _c0_run(int *p, int _) {
  *p = _c0_f(314, -29999927);
}

int _c0_main() {
  int* p = _c0_init(0);
  _c0_prepare(p, 0);
  _c0_run(p, 0);
  return _c0_checksum(p);
}

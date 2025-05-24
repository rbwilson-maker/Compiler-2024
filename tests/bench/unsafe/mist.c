#include <stdlib.h>

//test return 348930048

// gcc bench -O0 -> 10716139842 cycles on unix6
// gcc bench -O1 -> 897190056 cycles on unix6

int size() { return 64; }

int div2(int n) {
  if (n == 1 || n == 0) {
    return 0;
  }
  return 1 + div2(n / 2);
}

int mist(int i, int n) {
  int a = 1;
  int b = 2;
  int c = 5;
  int d = a + b * i + c * n;
  d *= (4 * 2 / 3) * 8 / 4;

  int kill_this = 0;
  for (int j = 0; j < i / 2; j++) {
    for (int l = 0; l < i / 4; l++) {
      kill_this += l * j;
    }
  }

  int magic = d;
  int x = magic * i + (128 * n) - magic;
  int y = (magic ^ 0x12401110) | 0x10251828;
  d = (magic & x) | y;
  magic ^= d;
  return magic;
}

int workUnit(int* a, int index, int n) {
  for (int num = 0; num < n; num++) {
    for (int i = 0; i < index; i++) {
      a[i] += div2(n * n * i);
      a[i] += mist(i, n);
      a[i] += mist(num * i, a[i]);
      for (int j = 0; j < i * num + index; j++) {
        a[i] *= mist(i, n);
      }
    }
  }
  return a[index] + size();
}

int* _c0_init(int _) { return calloc(sizeof(int), 1); }
void _c0_prepare(int* dat, int _) { *dat = 0; }
int _c0_checksum(int* p) { return *p; }

void _c0_run(int* p, int _) {
  int n = size();
  int* a = calloc(sizeof(int), n);
  int* b = calloc(sizeof(int), n);
  for (int i = 0; i < n; i++) {
    workUnit(a, i, n - i - 1);
    workUnit(b, n - i - 1, i);
  }

  int sum = 0;
  for (int i = 0; i < n; i++) {
    sum += a[i] * a[i];
  }

  for (int i = 0; i < n; i++) {
    sum += b[i] * b[i];
  }

  *p = sum;
}

int _c0_main() {
  int* p = _c0_init(0);
  _c0_prepare(p, 0);
  _c0_run(p, 0);
  return _c0_checksum(p);
}

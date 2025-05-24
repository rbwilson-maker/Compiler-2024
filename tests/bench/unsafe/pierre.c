//test return 15411
#include <stdbool.h>
#include <stdlib.h>
extern void _c0_assert(bool param);
// testing fermat's last theorem in SML

int _c0_pow(int acc, int x, int n) {
    if (n == 0) {
        return acc;
    }
    return _c0_pow(acc * x, x, n-1);
}

bool fermat(int n, int a, int b, int c) {
  if (a < 60) {
    if (b < 60) {
      if (c < 60) {
        if (_c0_pow(1, a, n) + _c0_pow(1, b, n) == _c0_pow(1, c,n)) {
          return false;
        }
        return fermat(n, a, b, c+1);
      }
      return fermat(n, a, b+1, 1);
    }
    return fermat(n, a+1, 1, 1);
  }
  return true;
}

int* _c0_init(int param) { return calloc(1, sizeof(int)); }
void _c0_prepare(int* p, int n) { *p = 0; }
int _c0_checksum(int* p) { return *p; }

int _c0_main() {
  if (!fermat(3,1,1,1)) {
    return 0;
  }
  return 15411;
}

void _c0_run(int* p, int n) { *p = _c0_main(); }


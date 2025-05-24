//test return 25
#include <stdbool.h>
#include <stdlib.h>
extern void _c0_assert(bool param);

int size() { return 2478; }

struct data_s{
  int result;
  int* x;
};
typedef struct data_s data_t;

int factorial(int n){
  if (n == 0) return 1;
  else return n * factorial(n-1);
}

data_t* _c0_init(int _){
  data_t* data = calloc(1, sizeof(data_t));
  data->result = 0;
  data->x = calloc(size(), sizeof(int));
  return data;
}

void _c0_prepare(data_t* data, int _) {
  data->result = 0;
  for(int i = 0; i < size(); i++)
    data->x[i] = 0;
}

int _c0_checksum(data_t *data, int _) {
   return data->result;
}

int mystery(int* x){
  for (int i = 0; i < size(); i++){
    x[i] = 1;
  }

  // Copies?
  for (int i = 0; i < size() / 100; i++){
    for (int y = size() - 1; y >= 0; y--){
      x[y] |= 0xFF & 32; 
      x[y] |= 0xFF & 4; 
      x[y] |= 0xFF & 1; 
      x[y] = factorial(x[y] * x[y]);
      x[y] /= x[y] + 1;
      x[y] = x[y] + x[i] + 1;
    }
  }

  return x[(size() * 3) / 4];
}

void _c0_run(data_t *data, int _){
  data->result = mystery(data->x);
}

int _c0_main(){
  int n = size();
  data_t *data = _c0_init(n);
  _c0_prepare(data, n);
  _c0_run(data, n);
  return _c0_checksum(data, n);
}

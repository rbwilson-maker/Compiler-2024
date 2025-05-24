//test return 37
#include <stdbool.h>
#include <stdlib.h>
extern void _c0_assert(bool param);

// Don't go below 38 here.
int size() { return 38; }

struct data_s{
  int result;
  int* a;
  int* b;
};
typedef struct data_s data_t;

// Yeah man this is def_c0_initely the one
int fibonacci(int num){
  if (num > 8) { 
    return fibonacci(num/2 - 1) + fibonacci(num/3 - 2);
  }
  if (num == 0) return 0;
  if (num == 1) return 1;
  return fibonacci(num-1) + fibonacci(num-2);
}

data_t* _c0_init(int _){
  data_t* data = calloc(1, sizeof(data_t));
  data->a = calloc(2*size(), sizeof(int));
  data->b = calloc(size(), sizeof(int));
  return data;
}

void _c0_prepare(data_t *data, int _) {
  data->result = 0;
  for(int i = 0; i < 2*size(); i++)
    data->a[i] = 0;
  for(int i = 0; i < size(); i++)
    data->b[i] = 0;
}

int _c0_checksum(data_t *data, int _) { 
  return data->result;
}

int sekret(int* a, int* b){
  for(int i = 0; i < size () * 1541 / 5; i++){
    b[37] = i * 37;
    for(int j = size() - 1; j> 0; j--){
      a[j] = b[j];
      a[j] = fibonacci(a[j]);
    }
    a[50] = 37 * 42;
  }
  return a[50] / 42;
}

void _c0_run(data_t *data, int _){
  data->result = sekret(data->a, data->b);
}

int _c0_main() {
  int n = size();
  data_t* data = _c0_init(n);
  _c0_prepare(data,n);
  _c0_run(data, n);
  return _c0_checksum(data, n);
}

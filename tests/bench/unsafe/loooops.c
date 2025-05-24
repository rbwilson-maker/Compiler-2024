#include <stdlib.h>

typedef struct data Data;

struct data {
  int result;
};


int _c0_fib(int n, int* p){
    if (n == 0 || n == 1){
        return 1;
    }

    for(int i = 0; i < 100; i++){
        if (n%2 == 0) {
            2+2;
        }
        else {
            5+5;
        }

    }
    return _c0_fib3(n-1) + _c0_fib3(n-2);
}

int _c0_fib2(int n, int* p){
    if (n == 0 || n == 1){
        return 1;
    }

    for(int i = 0; i < 3; i++){
        *p += 1;
        if (n%2 == 0) {
            2+2;
        }
        else {
            5+5;
        }

    }
    return _c0_fib2(n-1,p) + _c0_fib2(n-2,p);
}

int _c0_fib3(int n){
    if (n == 0 || n == 1){
        return 1;
    }

    for(int i = 0; i < 100; i++){
        if (n%2 == 0) {
            2+2;
        }
        else {
            5+5;
        }

    }
    return _c0_fib3(n-1) + _c0_fib3(n-2);
}

int loooooooooooops(){

    int x = 20;
    int a = 25;

    int* q = calloc(sizeof(int),1);
    for(int i = 0; i < 200; i++) { 
        if (_c0_fib(x,q) == 30) {
            return a;
        } 
        else {
            a += 1;
        }
    }
    
    int *b = calloc(sizeof(int),1);
    int *c = calloc(sizeof(int),1);
    int *d = calloc(sizeof(int),1);
    int *e = calloc(sizeof(int),1);
    int *f = calloc(sizeof(int),1);

    for(int i = 0; i < 50000000; i++) { 
        *b += i;
    }

    for(int i = 0; i < 50000000; i++) { 
        *c = i * *b + a;
        _c0_fib2(4,c);
    }

    for(int i = 0; i < 50000000; i++) { 
        *d = i;
        i += *d;
    }

    for(int i = 0; i < 50000000; i++) { 
        if (i%2 == 0){
            *e = i*3;
        } 
    }

    for(int i = 0; i < 50000000; i++) { 
        int k = i+1;
        int l = k+1;
        int m = l+1;
        int n = m+1;
        int o = n+1;
        *f += i + 2*k + l + 5*m + 3*n + 10*o;
    }

    int j = 0;
    int g = 0;
    int* p = calloc(sizeof(int),1);
    for(int i = 0; i < 20; i++) { 
        j += 2;
        g += _c0_fib2(j/2,p);
        g += j * *c + *d + *e + i * *f;
    }

    return a + *b + *c + *d + *e + *f + g + *p;
    
}


Data *_c0_init(int n) {
  Data *x = calloc(sizeof(Data), 1);
  return x;
}

void _c0_prepare(Data *d, int n) { return; }

void _c0_run(Data *d, int n) {
  int x = 11;
  d->result = loooooooooooops();
}

int _c0_checksum(Data *d, int n) {
  return d->result;
}

// Never actually called by the harness.
int _c0_main(){
  Data *p = _c0_init(0);
  _c0_prepare(p, 0);
  _c0_run(p, 0);
  return _c0_checksum(p, 0);
}
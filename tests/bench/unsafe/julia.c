//test return 544259544
#include <stdbool.h>
#include <stdlib.h>
extern void _c0_assert(bool param);
int size() { return 144; }

typedef int i32; 

typedef i32* image_t;

struct cmplx { 
  i32 r;
  i32 i;
}; 

typedef struct cmplx* cmplx_t; 

struct data {
  i32 dim;
  image_t image1; 
  image_t image2; 
  cmplx_t a; 
  cmplx_t c; 
  cmplx_t t;
};

typedef struct data Data;


// Standard library? What standard library?
i32 max(i32 a, i32 b) { return a > b ? a : b; }
i32 min(i32 a, i32 b) { return a < b ? a : b; }

// This isn't exactly the most efficient way to do this, especially
// as the whole struct can fit into 64 bytes, so the extra level of 
// indirection isn't too helpful...
void cmplx_mult(cmplx_t a, cmplx_t b, cmplx_t result){
  result->r = a->r * b->r - a->i * b->i; 
  result->i = a->i * b->r + a->r * b->i;
}

void cmplx_add(cmplx_t a,cmplx_t b, cmplx_t result){
  result->r = a->r + b->r;
  result->i = a->i + b->i;
}

// We do some basic flipping with cmplx numbers. This does 
// actually render the julia set. 
i32 julia (cmplx_t c, cmplx_t a, cmplx_t temp, i32 x, i32 y, i32 DIM) {
  i32 JULIA_ITERS = 128;
  i32 scale = 15000;
  i32 jx = scale * (DIM/2 - x)/(DIM/2);
  i32 jy = scale * (DIM/2 - y)/(DIM/2);
  
  c->r = -800; c->i = 156;
  a->r = jx;   a->i = jy;
  i32 i = 0;
  
  for (i = 0; i < JULIA_ITERS; i++) {
    cmplx_mult(a,a,temp); 
    cmplx_add(temp,c,a); // a = (a * a) + c;
  
    // Loop invariant dead code elimination (wow!)
    // If you don't eliminate, at least constant propagate.
    // Here, we'll throw in something for conditional information.
    for (i32 k = 1; k < scale / JULIA_ITERS; k *= 4) { 
      if (DIM == size()) {
        jx = scale * (DIM/2 - x)/(DIM/2); // DCE
        jx = scale * (DIM/2 - y)/(DIM/2); 
        jx = JULIA_ITERS * scale * (DIM/JULIA_ITERS - x) / (scale); 
        jx = JULIA_ITERS * scale * (DIM/JULIA_ITERS - x) / (scale); 
        // jx = temp->r * temp->i + temp->i - temp->r; // Hard mode
        jy = jx * jx * jx * jx;  // Aggressive DCE. 
      }
    }
  }

  i32 mag = a->r * a->r + a->i * a->i; 
  if (mag > 1000000)
    return 0;
  
  return mag / 255; // Scale down a bit.
}


// Strength reductions galore! See if you can optimize out
// the branch. It'll probably help.
i32 collatz(i32 n){
  i32 i = 0;
  while (n != 1) {
    if (n % 2 == 0) {
        n = n / 2;
    } else {
        n = (3 * n) + 1;
    }
    i++;
  }
  return i;
}

i32 recursive_collatz(i32 i, i32 n) {
  if (n == 1) return i;
  if (n % 2 == 0) 
    return recursive_collatz(i + 1,n / 2);
  return recursive_collatz(i + 1,(3 * n) + 1);
}


// Draw that julia-set fractal we mentioned earlier. 
// (If you need a test-case for your 15-418 final project, this is
// a particularly good one.)
image_t render_julia(Data* d, image_t image, i32 DIM){
  for (i32 y = 0; y < DIM; y++) {
    for (i32 x = 0; x < DIM; x++) {
      i32 offset = x + y * DIM;
      i32 juliaValue = julia(d->a, d->c, d->t, x, y, DIM);
      image[offset] = juliaValue;
    }
  }
  return image;
}

// Oh no, it's been collatz't! It's okay to collatz these values, 
// because they don't get very high. Well, offset does, sometimes, but
// we find that it all tends to work out in the end, as long as certain
// precautions are taken. Like putting in the constants we did.
image_t render_collatz(image_t image, image_t julia, i32 DIM) { 
  for (i32 i = 0; i < 50; i++){ // This loop is useless! (Time to hoist its contents...)
    for (i32 y = 0; y < DIM; y++) {
      for (i32 x = 0; x < DIM; x++) {
        i32 offset = (x + y * DIM);
        for (i32 k = 1; k < collatz(offset + 1); k *= 2) {
          // Loop invariance! Perhaps automatic memoization - but that won't work. Will it?
          i32 collatzValue = recursive_collatz(0,max(julia[offset],1));
          image[offset] += collatzValue - k; // This isn't. 
        }
      }
    // This is a pure function! (You can tell, because it doesn't ever dereference
    // memory, or do anything silly like divide by zero. You know, provably.) 
    collatz(max(julia[y], 1)); // As it happens, it's also dead code.
    }
  }
  return image;
}
 
// It may be useful to optimize this function.
void _c0_run_julia(Data* data) {
  i32 DIM = data->dim;
  image_t julia = render_julia(data,data->image1,DIM); 
  image_t collatz = render_collatz(data->image2,julia, DIM);
  data->image1 = julia; 
  data->image2 = collatz;
}

i32 xprod_arrays(image_t a, image_t b, i32 len) {
  i32 result = 0; 
  for (i32 i = 0; i < len; i++)
    result += a[i] ^ b[i]; 
  return result;
}


// Cycle Counting Interface Stuff. 
// Allocate the entire memory we'll ever use.
Data* _c0_init(int DIM){
  DIM = size(); 
  Data* data = calloc(1, sizeof(Data)); 
  data->dim = DIM;
  data->image1 = calloc(DIM*DIM, sizeof(i32));
  data->image2 = calloc(DIM*DIM, sizeof(i32));
  data->a = calloc(1, sizeof(struct cmplx));
  data->c = calloc(1, sizeof(struct cmplx));
  data->t = calloc(1, sizeof(struct cmplx));
  return data;
}

// Zero out any old memory.
void _c0_prepare(Data* data, int n){ 
  n = size();
  for (i32 i = 0; i < n; i++) { 
    data->image1[i] = 0; 
    data->image2[i] = 0; 
  }
}

int _c0_checksum(Data* data, int n){ 
  return xprod_arrays(data->image1,data->image2,data->dim);
}

void _c0_run(Data* data, int n) {
   _c0_run_julia(data); 
}

// Main function not called by harness.
int _c0_main(){ 
  i32 k = size();
  Data* p = _c0_init(k);
  _c0_prepare(p, k);
  _c0_run(p, k);
  return _c0_checksum(p,k);
}
//test return -4
#include <stdbool.h>
#include <stdlib.h>
extern void _c0_assert(bool param);

// Did you know that this program can be represented as a directed acyclic graph?
int size () { return 1024; }

// It's not really _c0_random. It actually generates quite a nice pixel pattern.
int _c0_randomize(int seed, int iterations) { 
  int n = 0;
  for (int i = 0; i < iterations; i++) { 
    n = seed; 
    seed = ((n>>1)+(1<<31)*(n%2))^(n|((n<<1)+((n>>31)%2)));
  }
  int sum = 0;
  for (int i = 0; i < 32; i++) { 
    sum += (n%2)/(1<<i);
    n >>= 1;
  }
  return sum;
}

void _c0_initialize(int* input, int x, int y, int iterations, int seed) { 
  for (int i = 0; i < x; i++) { 
    for (int j = 0; j < y; j++) { 
      seed = (seed^i)^j; 
      input[j*x+i] = _c0_randomize(seed,iterations);
    }
  }
}

// Naive Convolution (Square image)
void convolve(int* result, int* source, int* mask, int source_dim, int mask_dim, int total) {
  int off = mask_dim / 2; 
  for (int i = off; i < source_dim-off; i++){
    for (int j = off; j < source_dim-off; j++){
      int reduce_temp = 0; 

      int i0 = i-off; int i1 = i; int i2 = i + off;
      int j0 = j-off; int j1 = j; int j2 = j + off;

      if (mask_dim == 3) { 
        for(int k = 0; k < mask_dim * mask_dim; k++) { 
          int ik = k / mask_dim; int jk = k % mask_dim; 
          reduce_temp += source[ik*source_dim+jk] * mask[k];
        }
      } else { 
        for(int k = 0; k < mask_dim * mask_dim; k++) { 
          int ik = k / mask_dim; int jk = k % mask_dim; 
          reduce_temp += source[ik*source_dim+jk] * mask[k];
        }
      }
      if (total != 0) reduce_temp /= total;

      result[i0*(source_dim-mask_dim)+j0] = reduce_temp;
    }
  }
}

typedef struct mask Mask;
struct mask { 
  int* mask; 
  int dim; 
  int total; 
};

typedef struct data Data; 
struct data { 
  Mask* box; 
  Mask* gauss; 
  Mask* vertical; 
  Mask* edges; 
  Mask* sharpen; 
  int* image1;
  int* image2;
  Mask** masks;
};

// Box Blur =
//  {  .111, .111, .111, 
//     .111, .111, .111,
//     .111, .111, .111  };
Mask* box_blur() { 
  Mask* m = calloc(1, sizeof(Mask));
  m->mask = calloc(9, sizeof(int)); 
  for (int i = 0; i < 9; i++) m->mask[i] = 111; 
  m->dim = 3; 
  m->total = 1000;
  return m;
}

// Gaussian Blur = 
// {  0.060f, 0.130f, 0.060f, 
//    0.130f, 0.250f, 0.130f,  
//    0.060f, 0.130f, 0.060f };
Mask* gaussian_blur() { 
  Mask* m = calloc(1, sizeof(Mask));
  m->mask = calloc(9, sizeof(int)); 
  m->mask[0] =  60; m->mask[1] = 130; m->mask[2] = 60;
  m->mask[3] = 130; m->mask[4] = 250; m->mask[5] = 130;
  m->mask[6] =  60; m->mask[7] = 130; m->mask[8] = 60;
  m->dim = 3; 
  m->total = 1000;
  return m;
}

// Vertical Edge Detect = 
// {   1.f,  1.f ,  1.f,
//     0.f,  0.f ,  0.f,    
//    -1.f, -1.f , -1.f  };
Mask* vertical_edge(int n) { 
  Mask* m = calloc(1, sizeof(Mask));
  m->mask = calloc(n*n, sizeof(int)); 
  for (int i = 0; i < n; i++) { 
    m->mask[i] = 1; 
    m->mask[n-i-1] = -1;
  }
  m->dim = n; 
  return m;
}

// General Edge Detect =
// {  0.f,  1.f, 0.f, 
//    1.f, -8.f, 1.f, 
//    0.f,  1.f, 0.f  }; 
Mask* general_edge() { 
  Mask* m = calloc(1, sizeof(Mask));
  m->mask = calloc(9, sizeof(int)); 
                  m->mask[1] =  1; 
  m->mask[3] = 1; m->mask[4] = -8; m->mask[5] = 1;
                  m->mask[7] =  1; 
  m->dim = 3; 
  return m;
}

// Sharpen =
// {   0.f, -1.f,  0.f,
//    -1.f,  5.f, -1.f,
//     0.f, -1.f,  0.f  };
Mask* sharpen() { 
  Mask* m = calloc(1, sizeof(Mask));
  m->mask = calloc(9, sizeof(int));  
                   m->mask[1] = -1;
  m->mask[3] = -1; m->mask[4] =  5; m->mask[5] = -1;
                   m->mask[7] = -1; 
  m->dim = 3; 
  return m;
}


Data* _c0_init(int p) { 
  int n = size(); 
  Data* data = calloc(1, sizeof(Data)); 
  data->box = box_blur();
  data->gauss = gaussian_blur();
  data->vertical = vertical_edge(9);
  data->edges = general_edge();
  data->sharpen = sharpen();
  data->image1 = calloc(n*n, sizeof(int)); 
  data->image2 = calloc(n*n, sizeof(int)); 
  data->masks = calloc(5, sizeof(Mask*));
  return data;
}

void _c0_prepare(Data* data, int p) { 
  for (int i = 0; i < size() * size(); i++) { 
    data->image1[i] = 0; // Reset images
    data->image2[i] = 0;
  }
}


void _c0_run(Data* data,int p) {
  int n = size();
  Mask** masks = data->masks; 
  masks[0] = data->box; 
  masks[1] = data->gauss; 
  masks[2] = data->sharpen;
  masks[3] = data->vertical; 
  masks[4] = data->edges;

  _c0_initialize(data->image1,n,n,30,11347131);
  for (int i = 0; i < 5; i++){
    int* dest = i % 2 == 0 ? data->image2 : data->image1; 
    int* src  = i % 2 == 0 ? data->image1 : data->image2; 
    convolve(dest,src,masks[i]->mask,n,masks[i]->dim,masks[i]->total);
  }
}

int _c0_checksum(Data* data, int p) { 
  int result = 0;
  for (int i = 0; i < size() * size(); i++) { 
    if ((i % 4) == 0) { result += data->image1[i]; }
    if ((i % 4) == 1) { result ^= data->image2[i]; }
    if ((i % 4) == 2) { result -= data->image1[i]; }
    if ((i % 4) == 3 && data->image2[i] != 0) { result |= data->image2[i]; }
  }
  return result;
}

int _c0_main() { 
  int k = size(); 
  Data* d = _c0_init(k); 
  _c0_prepare(d,k);
  _c0_run(d,k);
  return _c0_checksum(d,k);
}
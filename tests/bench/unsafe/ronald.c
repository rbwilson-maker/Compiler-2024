//test return -166799318
#include <stdbool.h>
#include <stdlib.h>
extern void _c0_assert(bool param);

int size() { return 500000; }

int leftrotate(int x, int c) { // c == 24       c == 12
  int lo = x << c;       // i.e. 0x78000000 or 0x54321000;
  int hi = x >> (32-c);  // i.e. 0x00123456 or 0xFFFFF876
  int mask = ~(-1 << c); // i.e. 0x00FFFFFF or 0x00000FFF
  return lo | (hi & mask);
}

int flip(int x) {
  int y = 0;
  for (int i = 0; i < 4; i++) {
    y <<= 8;
    y |= x & 0xFF;
    x >>= 8;
  }
  return y;
}

// MD5 hashing algorithm
// Assumes message M has already been extended to a multiple of 512 bytes
// and that M is padded to as MD5 requires: the last two bytes encode
// the word,

int md5(int* s, int* K, int* M, int len) {
  _c0_assert(len % 16 == 0);
  int a0 = 0x67452301;
  int b0 = 0xefcdab89;
  int c0 = 0x98badcfe;
  int d0 = 0x10325476;

  for (int offset = 0; offset < len; offset += 16) {
    int A = a0;
    int B = b0;
    int C = c0;
    int D = d0;
    int F;
    int g;
    int shift_i;
    for (int i = 0; i < 64; i++) {
      if (i < 16) {
        F = (B & C) | (~B & D);
        g = i;
        shift_i = i%4;
      } else if (i < 32) {
        F = (D & B) | (~D & C);
        g = (5*i + 1) % 16;
        shift_i = i%4 + 4;
      } else if (i < 48) {
        F = B ^ C ^ D;
        g = (3*i + 5) % 16;
        shift_i = i%4 + 8;
      } else { 
        F = C ^ (B | ~D);
        g = (7*i) % 16;
        shift_i = i%4 + 12;
      }
      int Temp = D;
      D = C;
      C = B;
      B += leftrotate(A + F + K[i] + M[offset + g], s[shift_i]);
      A = Temp;
    }
    a0 += A;
    b0 += B;
    c0 += C;
    d0 += D;
  }
 
  flip(a0);
  flip(b0);
  flip(c0);
  flip(d0);
  return a0 ^ b0 ^ c0 ^ d0;
}

struct data {
  int* K;
  int* s;
  int* M;
  int n;
  int res;
};

// This is only called in _c0_init()
int* s() {
  int* s = calloc(16, sizeof(int));
  s[ 0] =  7; s[ 1] = 12; s[ 2] = 17; s[ 3] = 22; 
  s[ 4] =  5; s[ 5] =  9; s[ 6] = 14; s[ 7] = 20;
  s[ 8] =  4; s[ 9] = 11; s[10] = 16; s[11] = 23;
  s[12] =  6; s[13] = 10; s[14] = 15; s[15] = 21;
  return s;
}

// This is only called in _c0_init()
int* K() {
  int* K = calloc(64, sizeof(int));
  K[ 0]=0xd76aa478; K[ 1]=0xe8c7b756; K[ 2]=0x242070db; K[ 3]=0xc1bdceee;
  K[ 4]=0xf57c0faf; K[ 5]=0x4787c62a; K[ 6]=0xa8304613; K[ 7]=0xfd469501;
  K[ 8]=0x698098d8; K[ 9]=0x8b44f7af; K[10]=0xffff5bb1; K[11]=0x895cd7be;
  K[12]=0x6b901122; K[13]=0xfd987193; K[14]=0xa679438e; K[15]=0x49b40821;
  K[16]=0xf61e2562; K[17]=0xc040b340; K[18]=0x265e5a51; K[19]=0xe9b6c7aa;
  K[20]=0xd62f105d; K[21]=0x02441453; K[22]=0xd8a1e681; K[23]=0xe7d3fbc8;
  K[24]=0x21e1cde6; K[25]=0xc33707d6; K[26]=0xf4d50d87; K[27]=0x455a14ed;
  K[28]=0xa9e3e905; K[29]=0xfcefa3f8; K[30]=0x676f02d9; K[31]=0x8d2a4c8a;
  K[32]=0xfffa3942; K[33]=0x8771f681; K[34]=0x6d9d6122; K[35]=0xfde5380c;
  K[36]=0xa4beea44; K[37]=0x4bdecfa9; K[38]=0xf6bb4b60; K[39]=0xbebfbc70;
  K[40]=0x289b7ec6; K[41]=0xeaa127fa; K[42]=0xd4ef3085; K[43]=0x04881d05;
  K[44]=0xd9d4d039; K[45]=0xe6db99e5; K[46]=0x1fa27cf8; K[47]=0xc4ac5665;
  K[48]=0xf4292244; K[49]=0x432aff97; K[50]=0xab9423a7; K[51]=0xfc93a039;
  K[52]=0x655b59c3; K[53]=0x8f0ccc92; K[54]=0xffeff47d; K[55]=0x85845dd1;
  K[56]=0x6fa87e4f; K[57]=0xfe2ce6e0; K[58]=0xa3014314; K[59]=0x4e0811a1;
  K[60]=0xf7537e82; K[61]=0xbd3af235; K[62]=0x2ad7d2bb; K[63]=0xeb86d391;
  return K;
}

struct data* _c0_init(int _) {
  int n = size();
  int* A = calloc(16*n, sizeof(int));
  int x = 15;
  for (int i = 0; i < 16*n - 2; i++) {
    x = x * 1664525 + 1013904223;
    A[i] = x;
  }
  A[16*n - 2] = 512*n;

  struct data* D = calloc(1, sizeof(struct data));
  D->K = K();
  D->s = s();
  D->M = A;
  D->n = 16*n;
  return D;
}

void _c0_prepare(struct data* data, int _) {
  data->res = 0;
}

void _c0_run(struct data* data, int _) {
  data->res = md5(data->s, data->K, data->M, data->n);
}

int _c0_checksum(struct data* data, int _) {
  return data->res;
}

int _c0_main() {
  int n = size();
  struct data* D = _c0_init(n);
  _c0_prepare(D, n);
  _c0_run(D, n);
  return _c0_checksum(D, n);
}
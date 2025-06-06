//test return -1084304109
// gcc -O0: 7.320s. gcc -O1: 1.484s

#include <stdbool.h>
#include <stdlib.h>
extern void _c0_assert(bool param);

int size() { return 80000; }

/*
 * The following data structure is adapted from: 
 * An interface to unbounded arrays (15-122 Principles of Imperative Computation)
 */

int int_max() {
  return 2147483647;
}

typedef struct uba_header uba;
struct uba_header {
  int size;          // 0 <= size && size < limit
  int limit;         // 0 < limit
  int *data;     // \length(data) == limit
};

int uba_len(uba* A)
//@requires is_uba(A);
//@ensures 0 <= \result && \result < \length(A->data);
{
  return A->size;
}

int uba_get(uba* A, int i)
//@requires is_uba(A);
//@requires 0 <= i && i < uba_len(A);
{
  return A->data[i];
}

void uba_set(uba* A, int i, int x)
//@requires is_uba(A);
//@requires 0 <= i && i < uba_len(A);
//@ensures is_uba(A);
{
  A->data[i] = x;
}

uba* uba_new(int size)
//@requires 0 <= size;
//@ensures is_uba(\result);
//@ensures uba_len(\result) == size;
{
  uba* A = calloc(1, sizeof(uba));
  int limit = size == 0 ? 1 : size*2;
  A->data = (int *)(calloc(limit, sizeof(int)));
  A->size = size;
  A->limit = limit;

  return A;
}

void uba_resize(uba* A, int new_limit)
/* A may not be a valid array since A->size == A->limit is possible! */
//@requires A != NULL;
//@requires 0 <= A->size && A->size < new_limit;
//@requires \length(A->data) == A->limit;
//@ensures is_uba(A);
{
  int *B = (int *)(calloc(new_limit, sizeof(int)));

  for (int i = 0; i < A->size; i++)
    //@loop_invariant 0 <= i;
    {
      B[i] = A->data[i];
    }

  A->limit = new_limit;
  A->data = B;
}

void uba_add(uba* A, int x)
//@requires is_uba(A);
//@ensures is_uba(A);
{
  A->data[A->size] = x;
  (A->size)++;

  if (A->size < A->limit) return;
  _c0_assert(A->limit <= int_max() / 2); // Fail if array would get too big
  uba_resize(A, A->limit * 2);
}

int uba_rem(uba* A)
//@requires is_uba(A);
//@requires 0 < uba_len(A);
//@ensures is_uba(A);
{
  (A->size)--;
  int x = A->data[A->size];

  if (A->limit >= 4 && A->size <= A->limit / 4)
    uba_resize(A, A->limit / 2);

  return x;
}

// Client type
typedef uba* uba_t;

/*****************************Goldbach Test***********************************/

struct data_s{
  int result;
  bool *primes;
  uba_t real_primes;
};
typedef struct data_s data_t;
bool foo6(data_t *data, int x);

int decr(int num, int guard) {
  return guard % 2 > 0 ? num - 1 : num - 2;
}

int right_shift(int num, int guard) {
  return guard % 2 > 0 ? num >> 1 : num >> 2;
}

int left_shift(int num, int guard) {
  return guard % 2 > 0 ? num << 1 : num << 2;
}

int bit_and(int num, int guard) {
  return guard % 2 > 0 ? num & 15410 : num & 15411;
}

int bit_or(int num, int guard) {
  return guard % 2 > 0 ? num | 15410 : num | 15411;
}

int bit_xor(int num, int guard) {
  return guard % 2 > 0 ? num ^ 15410 : num ^ 15411;
}

int mult(int num, int guard) {
  return guard % 2 > 0 ? num * guard : num * guard * 2;
}

int _div(int num, int guard) {
  return guard % 2 > 0 ? num / 2 : num / 3;
}

int mod(int num, int guard) {
  return guard % 2 > 0 ? num % guard + 1 : num % guard - 1;
}

// this function looks interesting, what can help here?
int mystery(int num) {
  int i = 13;
  int j = 413;
  int k = 411;
  int l = 4;
  int m = 2;
  int n = 5;

  if ((num & 1) > 0) {
    // something might help here...
    i = _div(i, j);
    j = right_shift(j, k);
    k = mod(k, 15740);
    l = left_shift(l, m);
    m = decr(m, n);
    n += 2;
    if ((i + 2) * (j - 2) / k % 23 < 11 || ((j + 2) * (i - 2) / (m - 2) % 4 > 2)) {
      i = bit_xor(i, mult(i, j));
      j = bit_and(j, mult(j, k));
      k = bit_or(k, mult(k, m));
      m = mult(m, n);
      if ((i + 3) * (j ^ 31) / (k + 1) % (n - 1) >= (i - 3) * (j | 31) / (~k + 121) % (n - 1)) {
        i = (j + k + l + m) / n;
        j = (i ^ k ^ l ^ m) & n;
        k = (i | j | l | m) - n;
        if ((-i) / (j ^ 3) * (k + 1) % (n + 1) <= 3) {
          i = (k + j > 0) ? k + j : k - j;
          j = (k + i > 0) ? k + i : k - i;
          k = (j + i > 0) ? j + i : j - i;
          if ((~i << 1) / ((j + 15359) >> 3) * (k & 15410) % (n | 15411) < 15213) {
            if (i < j && j < k && k >= m && m >= n && (i > k || n < l)) {
              return num + 1;
            }
            return num + 2;
          }
          return num + 3;
        }
        return num + 4;
      } 
      return num + 5;
    } 
    return num + 6;
  } else {
    i = _div(i, j + 1);
    j = right_shift(j, k + 1);
    k = mod(k, 15746);
    l = left_shift(l, m + 1);
    m = decr(m, n + 1);
    n += 3;
    if ((i + 3) * (j - 3) / k % 27 < 13 || ((j + 3) * (i - 3) / (m - 3) % 6 > 3)) {
      i = bit_xor(i, mult(i, j + 1));
      j = bit_and(j, mult(j, k + 1));
      k = bit_or(k, mult(k, m + 1));
      if ((i + 31) * (j ^ 311) / (k + 11) % (n + 11) >= (i - 31) * (j | 311) / (~k + 321) % (n + 11)) {
        i = (j + k + l + m) / n;
        j = (i ^ k ^ l ^ m) & n;
        k = (i | j | l | m) - n;
        if ((-i) / (j ^ 4) * (k + 11) % (n + 11) <= 31) {
          i = (k + j > 0) ? k + j : k - j;
          j = (k + i > 0) ? k + i : k - i;
          k = (j + i > 0) ? j + i : j - i;
          if ((~i << 1) / ((j + 15112) >> 3) * (k & 15410) % (n | 15411) < 15213) {
            // i < j && j < k && k >= m && m >= n && (i > k || n < l)
            if (i < j && j < k && k >= m && m < n && (i > k || n >= l)) {
              return num + 1;
            }
            return num + 2;
          }
          return num + 3;
        }
        return num + 4;
      } 
      return num + 5;
    } 
    return num + 6;
  }
}

int abs(int x) {
  return x > 0 ? x : -x;
}

bool foo2(data_t *data, int x) {
  data->result += 1;
  return (data->result & 2) == 0;
}

bool foo3(data_t *data, int x) {
  return foo2(data, x) && (data->result & 4) == 0;
}

bool foo4(data_t *data, int x) {
  data->result += 2;
  return data->result % 4 == 0;
}

bool foo5(data_t *data, int x) {
  return data->result % 5 == 0 || foo6(data, abs(x)) || 1 >= 1 || foo4(data, x);
}

bool foo6(data_t *data, int x) {
  if (x >= 0) {
    return data->result % 6 >= 0;
  }
  return foo5(data, x) && data->result % 6 == 0;
}

bool foo7(data_t *data, int x) {
  return foo6(data, x) && data->result % 7 == 0;
}

void uba_add_wrapper(data_t *data, int x) {
  int sum1 = 3;
  int sum2 = 6;
  int iterations = sum1 + sum2;

  // what are you allowed to do here?
  for (int i = 0; i < iterations; i = mystery(i)) {
    for (int j = 0; j < iterations; j = mystery(j)) {
      for (int k = 0; k < iterations; k = mystery(k)) {
        sum1 = mystery(sum2);
        sum2 = mystery(sum1 * 2);
        foo3(data, x);
      }
    }
  }

  // what are you allowed to do here?
  for (int i = 0; i < iterations; i = mystery(i)) {
    for (int j = 0; j < iterations; j = mystery(j)) {
      for (int k = 0; k < iterations; k = mystery(k)) {
        sum1 += mystery(data -> result) + mystery(data -> result);
        sum2 += mystery(data -> result) * mystery(data -> result);
        foo7(data, x);
      }
    }
  }

  if (sum1 > 0) {
    sum1 *= 2;
    sum1 += sum1 + sum2;
  }
  
  if (sum2 > 0) {
    sum2 *= 2;
    sum2 += sum1 + sum2;
  }

  uba_add(data->real_primes, x);
}

bool is_prime(int x, data_t *data) {
  if (x % 2 == 0) {
    return false;
  }

  for (int i = 0; i < uba_len(data->real_primes); i++) {
    if (uba_get(data->real_primes, i) != 0 && x % uba_get(data->real_primes, i) == 0) {
      return false;
    }
  }
  uba_add_wrapper(data, x);
  return true;
}

bool goldbach(int num, data_t *data) {
  if (data->primes[2] && data->primes[num - 2]) {
    return true;
  }

  for (int i = 3; i <= num - i; i += 2) {
    if (data->primes[i] && data->primes[num - i]) {
      return true;
    }
  }
  return false;
}

data_t* _c0_init(){
  data_t* data = calloc(1, sizeof(data_t));
  data->primes = calloc(size() * 2, sizeof(bool));
  data->real_primes = uba_new(100);
  return data;
}

void _c0_prepare(data_t *data) {
  data->result = 1;
  data->primes[0] = false;
  data->primes[1] = false;
  data->primes[2] = true;
}

int _c0_checksum(data_t *data) { 
  return data->result;
}

int *foo(int *p) {
    *p += 1;
    return p;
}

// this exposes a potential bug
int copy() {
    int *p = calloc(1, sizeof(int));
    *p += 1;
    *p += *foo(p) * *foo(p);
    return *p;
}

void _c0_run(data_t *data){
  uba_add(data->real_primes, 2);
  for(int i = 3; i < size() * 2; i = mystery(i)) {
    data->primes[i] = is_prime(i, data);
  }

  for (int i = 2; i < size(); i = mystery(i)) {
    if (goldbach(i * 2, data)) {
      data->result += i;
    } else {
      // disproved the Goldbach Conjecture
      data->result = i * 2;
      return;
    }
  }
  data->result += copy();
}

int _c0_main() {
  int n = size();
  data_t* data = _c0_init();
  _c0_prepare(data);
  _c0_run(data);
  return _c0_checksum(data);
}

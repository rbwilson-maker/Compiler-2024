//test return 2144311707
#include <stdbool.h>
#include <stdlib.h>
extern void _c0_assert(bool param);

int size() { return 5000; }

// Priority queues, implemented as heaps
 
int elem_priority(int x) {
  return x;
}
typedef int elem;

int elem_priority(elem e);
typedef struct heap_header* pq;

bool pq_empty(pq P);		      // is P empty? 
bool pq_full(pq P);		        // is P full? 
void pq_insert(pq P, elem e);	// insert e into P 
elem pq_min(pq P);		        // find minimum 
elem pq_delmin(pq P);		      // delete minimum 

struct heap_header {
  int limit;			// limit = capacity+1 
  int next;			  // 1 <= next && next <= limit 
  elem* data;	  // \length(data) == limit 
};
typedef struct heap_header* heap;

struct _c0_rand {
  int seed;
};

typedef struct _c0_rand _c0_rand_t;

struct data { 
  int* A; 
  _c0_rand_t* gen; 
  heap H; 
  elem* heap_data; 
}; 
typedef struct data Data;

// priority(H, i) returns priority of element i in H
// it should not assume H is a heap
int priority(struct heap_header* H, int i) {
  return elem_priority(H->data[i]);
}

bool is_heap(struct heap_header* H) {
  if (!(H != NULL)) return false;
  if (!(1 <= H->next && H->next <= H->limit)) return false;
  for (int i = 2; i < H->next; i++)
    if (!(priority(H, i/2) <= priority(H, i))) return false;
  return true;
}

// H is a valid heap except possibly at n, looking up in the tree 
bool is_heap_except_up(heap H, int n) {
  if (H == NULL) return false;
  if (!(1 <= H->next && H->next <= H->limit)) return false;
  // check parent <= node for all nodes except root (i = 1) and n
  for (int i = 2; i < H->next; i++) {
      if (i != n && !(priority(H, i/2) <= priority(H, i)))
        return false;
      // for children of node n, check g_c0_randparent
      if (i/2 == n && (i/2)/2 >= 1 && !(priority(H, (i/2)/2) <= priority(H,i)))
        return false;
    }
  return true;
}

// H is a valid heap, except possibly at n,
// looking down in the tree 
// If 2*n >= H->next then is_heap_except_down(H, n) == is_heap(H) 
bool is_heap_except_down(heap H, int n) {
  if (H == NULL) return false;
  if (!(1 <= H->next && H->next <= H->limit)) return false;
  // check parent <= node for all nodes except root (i = 1)
  // and children of n (i/2 = n)
  for (int i = 2; i < H->next; i++){
    _c0_assert(2 <= i);
    if (i/2 != n && !(priority(H, i/2) <= priority(H, i)))
      return false;
    // for children of node n, check g_c0_randparent
    if (i/2 == n && (i/2)/2 >= 1 && !(priority(H, (i/2)/2) <= priority(H,i)))
      return false;
  }
  return true;
}

bool pq_empty(heap H) {
  return H->next == 1;
}

bool pq_full(heap H) {
  return H->next == H->limit;
}

heap pq_new(Data* data, int capacity){
  if (capacity > 0) {
    heap H = data->H;
    H->limit = size()+1;
    H->next = 1;
    H->data = data->heap_data;
    return H;
  } else {
    _c0_assert(false);
    return NULL;
  }
}

void swap(elem* A, int i, int j){ 
  elem tmp = A[i];
  A[i] = A[j];
  A[j] = tmp;
}

void pq_insert(heap H, elem e) { 
  H->data[H->next] = e;
  (H->next)++;
  // H is no longer a heap! 
  // ordering invariant could be violated at n 
  // re_c0_mainder could be pulled out as a function sift_up 
  int i = H->next - 1;
  while (i > 1 && priority(H,i) < priority(H,i/2)){
    swap(H->data, i, i/2);
    i = i/2;
  }
  return;
}

void sift_down(heap H, int i){ 
  int n = H->next;
  int left = 2*i;
  int right = left+1;
  while (left < n && 1 <= i && i < n && left == 2*i && right == 2*i+1) { 
      if (priority(H,i) <= priority(H,left) && 
         (right >= n || priority(H,i) <= priority(H,right)))
         return;
      if (right >= n || priority(H,left) < priority(H,right)) {
        swap(H->data, i, left);
        i = left;
      } else {
        swap(H->data, i, right);
        i = right;
      }
      left = 2*i;
      right = left+1;
    }
  return;
}

elem pq_delmin(heap H) { 
  int n = H->next;
  elem min = H->data[1];
  H->data[1] = H->data[n-1];
  H->next = n-1;
  if (H->next > 1) sift_down(H, 1);
  return min;
}

elem pq_min(heap H) {
  return H->data[1];
}

_c0_rand_t* _c0_init__c0_rand (Data* data, int seed) {
  data->gen->seed = seed;
  return data->gen;
}

int _c0_rand(_c0_rand_t* gen) {
  gen->seed = gen->seed * 1664525 + 1013904223;
  return gen->seed;
}

bool is_sorted(int* A, int lower, int upper) {
  if (0 <= lower && lower <= upper) { 
    for (int i = lower; i < upper-1; i++)
      if (!(A[i] <= A[i+1])) return false;
    return true;
  } else { 
    _c0_assert(false); 
    return false;
  }
}

Data* _c0_init(int _) {
  Data* data = calloc(1, sizeof(Data));  
  data->A = calloc(size(), sizeof(int));
  data->gen = calloc(1, sizeof(_c0_rand_t));
  data->H = calloc(1, sizeof(struct heap_header));
  data->heap_data = calloc(size()+1, sizeof(elem));
  return data; 
}

void _c0_prepare(Data* data, int _) {
  data->gen->seed = 0; 
  int n = size();
  for (int i = 0; i < n; i++) data->A[i] = 0;
}

void _c0_run(Data* data, int _) {
  int num_tests = 50;
  int n = size();
  _c0_rand_t* gen = _c0_init__c0_rand(data,0x8badf00d);
  int* A = data->A;
  for (int j = 0; j < num_tests; j++) {
    pq P = pq_new(data,n);
    for (int i = 0; i < n; i++) {
      _c0_assert(!pq_full(P));
      pq_insert(P, _c0_rand(gen));
      _c0_assert(!pq_empty(P));
    }
    _c0_assert(pq_full(P));
    for (int i = 0; i < n; i++) {
      _c0_assert(!pq_empty(P));
      A[i] = pq_delmin(P);
      _c0_assert(!pq_full(P));
    }
    _c0_assert(is_sorted(A, 0, n));
    _c0_assert(!pq_full(P));
    _c0_assert(pq_empty(P));
  }
}

int _c0_checksum(Data* data, int _) {
  return data->A[size() - 1];
}

int _c0_main() {
  int n = size();
  Data* data = _c0_init(n);
  _c0_prepare(data,n);
  _c0_run(data, n);
  return _c0_checksum(data, n);
}

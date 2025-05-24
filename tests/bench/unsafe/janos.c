//test return 3419999
#include <stdbool.h>
#include <stdlib.h>
extern void _c0_assert(bool param);

int size() { return 90000; }
typedef struct data Data;
struct wcount {
  int* word;
  int count;
};

typedef struct wcount* elem;
typedef int* key;

struct ht_header;
typedef struct ht_header* ht;

struct list_node {
  elem data;                    // data != NULL 
  struct list_node* next;
};
typedef struct list_node list;

struct ht_header {
  int size;                     // size >= 0 
  int capacity;                 // capacity > 0 
  list** table;                // \length(table) == capacity 
};

struct data { 
  int cur_word;
  int cur_list;
  int cur_count;
  int cur_elem;
  elem* counts;
  elem* elems;
  list** lists;
  int** words;
  ht H; 
  int _c0_checksum;
  list** table; 
};

// Hash tables (fixed size)
int hash_array(int* s, int limit){
  _c0_assert(limit > 1);
  int a = 1664525; int b = 1013904223; // inlined _c0_random number generator 
  int len = 4;                         // string_length(s);
  int h = 0;                           // empty string maps to 0 
  for (int i = 0; i < len; i++){
      h = h + s[i]; // char_ord(string_charat(s, i));
      h = h*a + b;  // mod 2^32, linear congruential _c0_random no 
  }

  // reduce to range 
  h = h % limit;
  _c0_assert(-limit < h && h < limit);
  if (h < 0) h = h + limit;
  return h;
}

int hash(int* s, int m) {
  return hash_array(s, m);
}

bool key_equal(int* s1, int* s2) {
  int len = 4;
  for (int i = 0; i < len; i++)
    if (s1[i] != s2[i]) return false;
  return true;
}

int* elem_key(struct wcount* wc) {
  return wc->word;
}

bool is_ht(ht H) {
  if (H == NULL) return false;
  if (!(H->size >= 0)) return false;
  if (!(H->capacity > 0)) return false;
  // check that each element of table is a valid chain 
  // includes checking that all elements are non-null 
  return true;
}

ht ht_new(Data* data){
  ht H = data->H;
  H->size = 0;
  H->capacity = size() / 5;
  H->table = data->table;
  return H;
}

// ht_lookup(H, k) returns NULL if key k not present in H 
elem ht_lookup(ht H, key k){
  int i = hash(k, H->capacity);
  list* p = H->table[i];
  while (p != NULL){
    if (key_equal(elem_key(p->data), k))
      return p->data;
    else
      p = p->next;
  }
  // not in list 
  return NULL;
}

list* next_list_node(Data* data) { 
  list* result = data->lists[data->cur_list];
  data->cur_list++;
  return result;
}

void ht_insert(Data* data,ht H, elem e) {
  _c0_assert(e != NULL);
  key k = elem_key(e);
  int i = hash(k, H->capacity);

  list* p = H->table[i];
  while (p != NULL){
    _c0_assert(p->data != NULL);
    if (key_equal(elem_key(p->data), k)){
      // overwrite existing element 
      p->data = e;
      return;
    } else {
      p = p->next;
    }
  }
  _c0_assert(p == NULL);
  // prepend new element 
  list* q = next_list_node(data);
  q->data = e;
  q->next = H->table[i];
  H->table[i] = q;
  (H->size)++;
  return;
}

// Debugging: get the length of a chain
int ht_chain_length(list* C) {
  int i = 0;
  while (C != NULL) {
    i++;
    C = C->next;
  }
  return i;
}

int* next_word(Data* data) { 
  int* result = data->words[data->cur_word];
  data->cur_word++;
  return result;
}

elem new_elem(Data* data) { 
  elem result = data->elems[data->cur_elem];
  data->cur_elem++;
  return result;
}

int* _c0_init_word(Data* data, int i, int j, int n) {
  int* A = next_word(data);
  A[0] = j*n+i; A[1] = i*j+n; A[2] = n*i+j; A[3] = i*i+j*j;
  return A;
}

Data* _c0_init(int _) {
  Data* data = calloc(1, sizeof(Data)); 
  int k = 5;
  data->counts = calloc(k*size(), sizeof(elem));
  data->words = calloc(k*size(), sizeof(int*));
  data->elems = calloc(k*size(), sizeof(elem));
  data->lists = calloc(k*size(), sizeof(list*));
  for (int i = 0; i < k*size(); i++) { 
    data->words[i] = calloc(4, sizeof(int));
    data->counts[i] = calloc(1, sizeof(struct wcount)); 
    data->lists[i] = calloc(1, sizeof(list));
    data->elems[i] = calloc(1, sizeof(struct wcount));
  }
  data->H = calloc(1, sizeof(struct ht_header));
  data->table = calloc((size()/5) + 1, sizeof(list*));
  return data;
}

void _c0_prepare(Data* data, int _) {
  data->_c0_checksum = 0;
  for (int i = 0; i < 2*size(); i++) { 
    //data->counts[i]->word = NULL; 
    data->counts[i]->count = 0;
    data->elems[i]->count = 0; 
  }
  data->cur_word = 0;
  data->cur_count = 0; 
  data->cur_elem = 0; 
  data->cur_list = 0;
}

void _c0_run(Data* data, int _) {
  int n = size();
  int j = 37;
  ht H = ht_new(data);  // table will end up with load factor 5 
  for (int i = 0; i < n; i++) {
    elem e = new_elem(data);
    e->word = _c0_init_word(data,i,j,n);
    e->count = j*n+i;
    ht_insert(data, H, e);
  }
  for (int i = 0; i < n; i++) {
    // missing existing element? 
    _c0_assert(ht_lookup(H, _c0_init_word(data, i,j,n))->count == j*n+i);
  }
  for (int i = 0; i < n; i++) {
    // finding nonexistent element? 
    _c0_assert(ht_lookup(H, _c0_init_word(data,i+1,j+1,n)) == NULL);
  }
  data->_c0_checksum = ht_lookup(H, _c0_init_word(data,n-1, 37, n))->count;
}

int _c0_checksum(Data* data, int _) {
  return data->_c0_checksum;
}

int _c0_main() {
  int n = size();
  Data* data = _c0_init(n);
  _c0_prepare(data, n);
  _c0_run(data, n);
  return _c0_checksum(data, n);
}

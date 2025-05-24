#ifndef _SYMBOLS_H_
#define _SYMBOLS_H_

#include <bits/stdc++.h> 
using namespace std; 

typedef const char* Symbol;

// all identifiers are stored once and only once so in rest of
// compiler we can just compare pointers to determine if two
// identifiers are the same.

class Symbols {
 private:
  struct SymHash {
  public:
    size_t operator()(const char* const& sym) const {
      size_t h = 0;
      for (const char* p=sym; *p; p++) h = (h<<1)^*p;
      return h;
    }
  };
  struct SymDeepEqual {
  public:
    bool operator()(const char* const& str1, const char* const& str2) const {
      const char* p=str1;
      const char* q=str2; 
      while (*p && *q && *p == *q) {
	p++;
	q++;
      }
      return (*p == 0)&&(*q == 0);
    }
  };    
  static unordered_set<const char*, SymHash, SymDeepEqual> symbols;
 public:
  static Symbol intern(const char* str);
};

#endif

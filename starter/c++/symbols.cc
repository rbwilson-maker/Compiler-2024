#include "symbols.h"
#include <stdlib.h>

unordered_set<const char*, Symbols::SymHash, Symbols::SymDeepEqual> Symbols::symbols;

Symbol Symbols::intern(const char* str) {
  auto already = symbols.find(str);
  if (already != symbols.end())
    return (*already);
  auto result = symbols.insert(strdup(str));
  return *(result.first);
}

#if 0
#include <stdio.h>
// test out interning symbols
int
main(int argc, char** argv) {
  for (int i=0; i<argc; i++) {
    Symbol s = Symbols::intern(argv[i]);
    printf("%p %s\n", s, s);
  }
  return 0;
}
#endif

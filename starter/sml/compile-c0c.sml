(* L1 Compiler
 * Helper for compilation
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 *)

CM.make "sources.cm";
SMLofNJ.exportFn ("../bin/c0c.heap", Top.main);

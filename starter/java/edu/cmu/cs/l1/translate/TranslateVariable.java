// Mike Maxim
// Variable translator

package edu.cmu.cs.l1.translate;

import edu.cmu.cs.l1.general.*;
import edu.cmu.cs.l1.absyn.*;
import edu.cmu.cs.l1.symbol.Symbol;
import edu.cmu.cs.l1.errormsg.*;
import edu.cmu.cs.l1.tree.*;
import java.util.List;

public
class TranslateVariable extends Translate {
 public
  TranslateVariable(ErrorMsg m, TranslateEnvironment t) { super(m, t); }

 public
  void visitResponse(String type, Object obj) {
    try {
      if (type.equals("ASSimpleLval")) {
        visitSimpleVar(obj);
      } else {
        System.out.println("INTERNAL COMPILER ERROR: Skipping: " + type);
      }
    } catch (TranslateException e) {
      System.out.println("TRANSLATE: " + e.getMessage());
      Globals.shutdown(-1);
    }
  }

 public
  void visitSimpleVar(Object v) throws TranslateException {
    ASSimpleLval av = (ASSimpleLval)v;

    if (null == (m_var = m_table.getEntry(av.getName()))) {
      error(av.getPosition(), "variable used without declaration");
      throw new TranslateException("STOPPING");
    }

    /* Removing this check, as it causes legal programs like
     * int main () { int x; return 4; int y = x; }
     * to be rejected. - RJS September 7, 2015
    if (!m_table.isInitialized(av.getName())) {
        error(av.getPosition(), "variable used before initialization");
        throw new TranslateException("STOPPING");
    } */
  }

 public
  IRExpression getVariable() { return m_var; }

 private
  IRExpression m_var;
 private
  Symbol m_symbol;
}

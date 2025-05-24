// Mike Maxim
// Absyn for a variable declaration

package edu.cmu.cs.l1.absyn;

import edu.cmu.cs.l1.symbol.*;
public
class ASDeclStatement extends ASStatement {
  /** Construct a declaration from a variable
   * @param p Position in source
   * @param vname Variable name
   */
 public
  ASDeclStatement(int p, Symbol vname) {
    super(p);
    v_name = vname;
    m_classname = "ASDeclStatement";
  }

  /** Construct a declaration from a variable assigned to an expression
    * @param p Position in source
    * @param vname Variable name
    * @param expr Expression.
    */
 public
  ASDeclStatement(int p, Symbol vname, ASExpression expr) {
    super(p);
    m_classname = "ASDeclStatement";
    v_name = vname;
    v_expr = expr;
  }

  /** Return the symbol for the variable */
 public
  Symbol getVar() { return v_name; }

  /** Return the expression for the variable */
 public
  ASExpression getExpression() { return v_expr; }

  /** Check if declaration has an assignment */
 public
  Boolean hasAssignment() { return v_expr != null; }

 private
  Symbol v_name;
 private
  ASExpression v_expr = null;
}

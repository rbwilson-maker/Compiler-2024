// Mike Maxim
// Absyn for hexadecimal expressions.

package edu.cmu.cs.l1.absyn;

public
class ASHexConstant extends ASExpression {
  /** Construct the integer constant
   * @param p The position in the source
   * @param v Value of the hex
   */
 public
  ASHexConstant(int p, int v) {
    super(p);
    m_value = v;
    m_classname = "ASHexConstant";
  }

 public
  ASHexConstant(int p, long v) {
    super(p);
    m_value = v;
    m_classname = "ASHexConstant";
  }

  /** Construct an int with a String parameter
   * @param p Position in source
   * @param v String form of the number
   */
 public
  ASHexConstant(int p, String v) {
    super(p);

    m_classname = "ASHexConstant";

    if (v.length() < 3) {
      m_value = 0;
      return;
    }
    m_value = Long.parseLong(v.substring(2, v.length()), 16);
  }

  /** Return the value */
 public
  long getValue() { return m_value; }

  /** Return value in hex format */
 public
  String toString() { return "0x" + Long.toHexString(m_value); }

 private
  long m_value;
}

// Mike Maxim
// IR Expression translator

package edu.cmu.cs.l1.translate;

import edu.cmu.cs.l1.general.*;
import edu.cmu.cs.l1.symbol.Symbol;
import edu.cmu.cs.l1.temp.Label;
import edu.cmu.cs.l1.absyn.*;
import edu.cmu.cs.l1.general.*;
import edu.cmu.cs.l1.errormsg.*;
import edu.cmu.cs.l1.tree.*;

public
class TranslateExpression extends Translate {
 public
  TranslateExpression(ErrorMsg m, TranslateEnvironment t) { super(m, t); }

 public
  IRExpression asExpression() { return m_exp; }

 public
  void visitResponse(String type, Object obj) {
    if (type.equals("ASIntConstant")) {
      visitIntExp(obj);
    } else if (type.equals("ASHexConstant")) {
      visitHexExp(obj);
    } else if (type.equals("ASOpExpression")) {
      visitOpExp(obj);
    } else if (type.equals("ASLvalExpression")) {
      visitVarExp(obj);
    } else {
      System.out.println("INTERNAL COMPILER ERROR: Skipping: " + type);
    }
  }

  /** Visit an int */
 public
  void visitIntExp(Object i) {
    ASIntConstant ai = (ASIntConstant)i;

    // This checks for bad integer constants
    if (ai.getValue() > ((long)2147483647) + 1 || ai.getValue() < -2147483648) {
      m_errorMsg.error(ai.getPosition(),
                       "TRANSLATE: int constant out of range");
      System.exit(-1);
    }

    m_exp = new IRConst(ai.getValue());
  }

  /** Visit an hex */
 public
  void visitHexExp(Object h) {
    ASHexConstant ah = (ASHexConstant)h;

    // Check for valid unsigned integer constants
    if (ah.getValue() >= ((long)4294967295L) + 1L || ah.getValue() < 0) {
      m_errorMsg.error(ah.getPosition(),
                       "TRANSLATE: hex constant out of range");
      System.exit(-1);
    }

    m_exp = new IRConst(ah.getValue());
  }

  /** Visit an OpExpression */
 private
  void visitOpExp(Object i) {
    ASOpExpression ai = (ASOpExpression)i;

    TranslateExpression l = new TranslateExpression(m_errorMsg, m_table);
    TranslateExpression r = null;

    if (ai.getLeft() != null) {
      ai.getLeft().visit(l);
    }

    if (ai.getRight() != null) {
      r = new TranslateExpression(m_errorMsg, m_table);
      ai.getRight().visit(r);
    }

    switch (ai.getOperator()) {
      case ASOpExpression.PLUS:
        m_exp = new IRBinop(IRBinop.PLUS, l.asExpression(), r.asExpression());
        break;
      case ASOpExpression.MINUS:
        m_exp = new IRBinop(IRBinop.MINUS, l.asExpression(), r.asExpression());
        break;
      case ASOpExpression.MUL:
        m_exp = new IRBinop(IRBinop.MUL, l.asExpression(), r.asExpression());
        break;
      case ASOpExpression.DIV:
        m_exp = new IRBinop(IRBinop.DIV, l.asExpression(), r.asExpression());
        break;
      case ASOpExpression.MOD:
        m_exp = new IRBinop(IRBinop.MOD, l.asExpression(), r.asExpression());
        break;
      case ASOpExpression.NEG:
        m_exp = new IRBinop(IRBinop.MINUS, new IRConst(0), l.asExpression());
        break;
      default:
        System.out.println("WARNING: Unable to process: " + ai.getOperator());
    }
  }

  /** Visit a LValExp */
 private
  void visitVarExp(Object v) {
    ASLvalExpression av = (ASLvalExpression)v;

    TranslateVariable varTranslator =
        new TranslateVariable(m_errorMsg, m_table);
    av.getVariable().visit(varTranslator);

    m_exp = varTranslator.getVariable();
  }

 private
  IRExpression m_exp;
}

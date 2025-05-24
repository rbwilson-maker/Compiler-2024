// Mike Maxim
// Statement translator

package edu.cmu.cs.l1.translate;

import edu.cmu.cs.l1.general.*;
import edu.cmu.cs.l1.absyn.*;
import edu.cmu.cs.l1.symbol.*;
import edu.cmu.cs.l1.temp.Temp;
import java.util.*;
import edu.cmu.cs.l1.errormsg.*;
import edu.cmu.cs.l1.tree.*;
import edu.cmu.cs.l1.temp.*;

public
class TranslateStatement extends Translate {
 public
  TranslateStatement(ErrorMsg e, TranslateEnvironment t) { super(e, t); }

 public
  IRStatement getStatement() { return m_stm; }

 public
  void visitResponse(String type, Object obj) {
    int pos = 0;

    if (obj instanceof ASStatement) {
      pos = ((ASStatement)obj).getPosition();
    } else if (obj instanceof ASExpression) {
      pos = ((ASExpression)obj).getPosition();
    } else if (obj instanceof ASLval) {
      pos = ((ASLval)obj).getPosition();
    }

    try {
      if (type.equals("ASAssignStatement")) {
        visitAssign(obj);
      } else if (type.equals("ASStatementList")) {
        visitStmList(obj);
      } else if (type.equals("ASReturnStatement")) {
        visitReturn(obj);
      } else if (type.equals("ASDeclStatement")) {
        visitDecl(obj);
      } else {
        System.out.println("INTERNAL COMPILER ERROR: Skipping: " + type);
      }
    } catch (Exception e) {
      m_errorMsg.error(pos, "TRANSLATE: " + e.getMessage());
      System.exit(-1);
    }
  }

  /** Visit a return */
 private
  void visitReturn(Object o) {
    ASReturnStatement ao = (ASReturnStatement)o;
    IRStatement presult;

    TranslateExpression retran = new TranslateExpression(m_errorMsg, m_table);
    ao.getReturnExpression().visit(retran);
    m_stm = new IRReturn(retran.asExpression());
  }

  /** Visit a declaration */
 private
  void visitDecl(Object o) throws SymbolTableException {
    ASDeclStatement decl = (ASDeclStatement)o;
    if (m_table.getEntry(decl.getVar()) == null)
      m_table.addEntry(decl.getVar(), new IRTemp(new Temp()));
    else {
      m_errorMsg.error(decl.getPosition(),
                       "TRANSLATE: same variable name declared twice");
      System.exit(-1);
    }

    if (decl.hasAssignment()) {
      TranslateVariable vhs = new TranslateVariable(m_errorMsg, m_table);
      TranslateExpression rhs = new TranslateExpression(m_errorMsg, m_table);

      decl.getExpression().visit(rhs);

      // variable is now initialized.
      m_table.markInitialized(decl.getVar());
      ASSimpleLval variable =
          new ASSimpleLval(decl.getPosition(), decl.getVar());
      variable.visit(vhs);

      m_stm = new IRMove(vhs.getVariable(), rhs.asExpression());
    } else {
      // if this is a simple declaration, there is no code generated.
      m_stm = null;
    }
  }

  /** Visit an assign */
 private
  void visitAssign(Object s) throws TranslateException, SymbolTableException {
    ASAssignStatement as = (ASAssignStatement)s;

    TranslateExpression rhs = new TranslateExpression(m_errorMsg, m_table);
    TranslateVariable vhs = new TranslateVariable(m_errorMsg, m_table);
    as.getExpression().visit(rhs);

    ASSimpleLval var = (ASSimpleLval)as.getVariable();
    if (m_table.getEntry(var.getName()) == null) {
      m_errorMsg.error(var.getPosition(),
                       "TRANSLATE: variable assigned to before being declared");
      System.exit(-1);
    }

    m_table.markInitialized(var.getName());

    as.getVariable().visit(vhs);

    m_stm = new IRMove(vhs.getVariable(), rhs.asExpression());
  }

  /** Visit a stm list */
 private
  void visitStmList(Object s) {
    ASStatementList ss = (ASStatementList)s;

    if (ss.getSize() == 0) {
      m_stm = new IRLabel(new Label());
      return;
    }

    TranslateStatement h = new TranslateStatement(m_errorMsg, m_table);
    TranslateStatement t = new TranslateStatement(m_errorMsg, m_table);

    if (ss.getSize() > 1) {
      ASStatementList tail = ss.removeFront();
      ss.getStatement(0).visit(h);
      tail.visit(t);

      if (h.getStatement() == null && t.getStatement() == null)
        m_stm = null;
      else if (h.getStatement() == null)
        m_stm = t.getStatement();
      else if (t.getStatement() == null)
        m_stm = h.getStatement();
      else
        m_stm = new IRSeq(h.getStatement(), t.getStatement());
    } else {
      ss.getStatement(0).visit(h);

      m_stm = h.getStatement();
    }
  }

 private
  IRStatement m_stm;
 private
  static Stack m_donestack = new Stack(), m_teststack = new Stack();
}

package slacc
package analyzer

import slacc.analyzer.Types._
import slacc.ast.Trees._
import slacc.utils._

object TypeChecking extends Pipeline[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      def csTypeOfExpr(expr: ExprTree): Type = {
        expr match {

          case and: And =>
            tcExpr(and.lhs, TBoolean)
            tcExpr(and.rhs, TBoolean)
            and.getType

          case or: Or =>
            tcExpr(or.lhs, TBoolean)
            tcExpr(or.rhs, TBoolean)
            or.getType

          case lt: LessThan =>
            tcExpr(lt.lhs, TInt)
            tcExpr(lt.rhs, TInt)
            lt.getType


          case eq: Equals =>
            tcExpr(eq.lhs, TInt)
            tcExpr(eq.rhs, TInt)
            eq.getType

          case p: Plus =>
            tcExpr(p.lhs, TInt, TString)
            tcExpr(p.rhs, TInt, TString)
            p.getType

          case mi: Minus =>
            tcExpr(mi.lhs, TInt)
            tcExpr(mi.rhs, TInt)
            mi.getType

          case mul: Times =>
            tcExpr(mul.lhs, TInt)
            tcExpr(mul.rhs, TInt)
            mul.getType

          case div: Div =>
            tcExpr(div.lhs, TInt)
            tcExpr(div.rhs, TInt)
            div.getType

          case arrRead: ArrayRead =>
            tcExpr(arrRead.arr, TIntArray)
            tcExpr(arrRead.index, TInt)
            arrRead.getType

          case arrLen: ArrayLength =>
            tcExpr(arrLen.arr, TIntArray)
            arrLen.getType

          case mc: MethodCall =>
            tcExpr(mc.obj)
            val callee = mc.obj.getType.asInstanceOf[TClass].classSymbol.methods get mc.meth.value
            mc.meth.setSymbol(callee.get)
            mc.setType(mc.meth.getType)
            mc.getType

          case intLit: IntLit =>
            intLit.getType

          case strLit: StringLit =>
            strLit.getType

          case t: True =>
            t.getType

          case f: False =>
            f.getType

          case id: Identifier =>
            id.getType

          case s: Self =>
            s.setType(s.getSymbol.getType)
            s.getType

          case nia: NewIntArray =>
            nia.getType

          case n: New =>
            n.setType(n.cls.getType)
            n.getType

          case not: Not =>
            tcExpr(not.expr, TBoolean)
            not.getType

          case b: Block =>
            for (e <- b.exprs) {
              tcExpr(e)
            }
            b.getType

          case i: If =>
            tcExpr(i.cond, TBoolean)
            tcExpr(i.thn)
            if (i.els.isDefined) {
              tcExpr(i.els.get)
            }
            i.getType

          case w: While =>
            tcExpr(w.cond, TBoolean)
            tcExpr(w.body)
            w.getType

          case pr: Println =>
            tcExpr(pr.expr, TString, TInt, TBoolean)
            pr.getType

          case ass: Assign =>
            tcExpr(ass.expr, ass.id.getType)
            ass.id.getSymbol.setType(ass.expr.getType)
            ass.setType(ass.id.getType)
            ass.getType

          case arrAss: ArrayAssign =>
            tcExpr(arrAss.id, TIntArray)
            tcExpr(arrAss.index, TInt)
            tcExpr(arrAss.expr, TInt)
            arrAss.setType(arrAss.id.getType)
            arrAss.getType

          case strOf: StrOf =>
            tcExpr(strOf.expr, TString, TInt)
            strOf.getType

          case _ =>
            TError
        }
      }
      // TODO: Compute type for each kind of expression
      val tpe: Type = csTypeOfExpr(expr)

      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } else {
        tpe
      }
    }

    def csMethodDeclTypes(md: MethodDecl): Unit = {
      for (e <- md.exprs) {
        tcExpr(e)
      }
    }

    for (classDecl <- prog.classes) {

      for (methodDecl <- classDecl.methods) {
        csMethodDeclTypes(methodDecl)
      }
    }

    csMethodDeclTypes(prog.main.main)

    prog
  }

}

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
      val tpe: Type = {
        // TODO: Compute type for each kind of expression
        expr match {
          case And(_,_) | Or(_,_) | LessThan(_,_) | Equals(_,_) | True() | False() | Not(_) =>
            TBoolean

          case Plus(_,_) | Minus(_,_) | Times(_,_) | Div(_,_) =>
            TInt

          case ArrayRead(arr, index) =>
            arr.getType

          case ArrayLength(_) =>
            TInt

          case MethodCall(obj, meth, argList) =>
            TUnit

          case IntLit(_) =>
            TInt

          case StringLit(_) =>
            TString

          case Identifier(id) =>
            expr.getType

          case Self() =>
            expr.getType

          case NewIntArray(_) =>
            TIntArray

          case New(tpe) =>
            tpe.getType

          case Block(exprs) =>
            TUnit

          case If(cond, thn, els) =>
            TUnit

          case While(cond, body) =>
            TUnit

          case Println(_) =>
            TString

          case Assign(id, expr) =>
            id.getType

          case ArrayAssign(id, index, expr) =>
            id.getType

          case StrOf(_) =>
            TString

          case _ =>
            TUnit
        }
      }


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

    prog
  }

}

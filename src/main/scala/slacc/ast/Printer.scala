package slacc
package ast

import Trees._
import slacc.analyzer.Symbols._
import utils._

object Printer {
  def apply(ctx: Context)(t: Tree): String = {
    val defaultIndent = "  "
    val numOfFirstLevelIndents = 1

    def getSymbolStr(symbol: Symbol): String = {
      if (ctx.doSymbolIds) {
        "#" + symbol.id
      }
      else {
        ""
      }
    }

    def processClassStr(classDecl: ClassDecl) : String = {
      val sb = new StringBuilder()

      sb.append("class ")
      sb.append(classDecl.getName)
      sb.append(getSymbolStr(classDecl.getSymbol))

      if (classDecl.parent.isDefined) {
        val parent = classDecl.parent
        sb.append(" <: " + parent.get.value)
        sb.append(getSymbolStr(parent.get.getSymbol))
      }
      sb.append(" {\n")


      for (varItr <- classDecl.vars) {
        sb.append(defaultIndent)
        sb.append(processVarStr(varItr))
        sb.append(";\n")
      }

      for (methodItr <- classDecl.methods) {
        sb.append(processMethodStr(methodItr, numOfFirstLevelIndents))
        sb.append("\n")
      }
      sb.append("}\n\n")
      sb.toString
    }

    def processMethodStr(methodDecl: MethodDecl, initialIndentCount: Int = 0) : String = {
      val sb: StringBuilder = new StringBuilder()

      val methodName = methodDecl.id.value
      val args = methodDecl.args
      val vars = methodDecl.vars
      val exprs = methodDecl.exprs
      val retType = methodDecl.retType

      sb.append(getNumIndents(initialIndentCount))
      sb.append("method " + methodName)
      sb.append(getSymbolStr(methodDecl.getSymbol))
      sb.append("(" + processFormalStr(args) + ") : ")
      sb.append(processTypeStr(retType) +" = {\n")

      val indentCount = initialIndentCount+1
      for (varItr <- vars) {
        sb.append(getNumIndents(indentCount))
        sb.append(processVarStr(varItr))
        sb.append(";\n")
      }

      for (expr <- exprs) {
        sb.append(getNumIndents(indentCount))
        sb.append(processExprStr(expr, indentCount))
        sb.append(";\n")
      }

      sb.setLength(sb.length-2)
      sb.append("\n"+ getNumIndents(initialIndentCount) +"}")
      sb.toString()
    }

    def processExprStr(exprDecl: ExprTree, initialIndentCount: Int) : String = {
      val sb: StringBuilder = new StringBuilder()

      exprDecl match {
        case and: And =>
          "(" +
            processExprStr(and.lhs, initialIndentCount) +
            " && " +
            processExprStr(and.rhs, initialIndentCount) +
            ")"

        case or: Or =>
          "(" +
            processExprStr(or.lhs, initialIndentCount) +
            " || " +
            processExprStr(or.rhs, initialIndentCount) +
            ")"

        case plus: Plus =>
          "(" +
            processExprStr(plus.lhs, initialIndentCount) +
            " + " +
            processExprStr(plus.rhs, initialIndentCount) +
            ")"

        case minus: Minus =>
          "(" +
            processExprStr(minus.lhs, initialIndentCount) +
            " - " +
            processExprStr(minus.rhs, initialIndentCount) +
            ")"

        case times: Times =>
          "(" +
            processExprStr(times.lhs, initialIndentCount) +
            " * " +
            processExprStr(times.rhs, initialIndentCount) +
            ")"

        case div: Div =>
          "(" +
            processExprStr(div.lhs, initialIndentCount) +
            " / " +
            processExprStr(div.rhs, initialIndentCount) +
            ")"

        case lt: LessThan =>
          processExprStr(lt.lhs, initialIndentCount) +
            " < " +
            processExprStr(lt.rhs, initialIndentCount)

        case eq: Equals =>
          processExprStr(eq.lhs, initialIndentCount) +
            " == " +
            processExprStr(eq.rhs, initialIndentCount)

        case arrRead: ArrayRead =>
          processExprStr(arrRead.arr, initialIndentCount) +
            "[" + processExprStr(arrRead.index, initialIndentCount) + "] "

        case arrLen: ArrayLength =>
          processExprStr(arrLen.arr, initialIndentCount) + ".length"

        case methodCall: MethodCall =>
          sb.append(processExprStr(methodCall.obj, initialIndentCount))
          sb.append(".")
          sb.append(processExprStr(methodCall.meth, initialIndentCount))
          sb.append("(")

          val params = methodCall.args
          for (param <- params) {
            sb.append(processExprStr(param, initialIndentCount + 1))
            sb.append(", ")
          }

          sb.setLength(sb.length-2)
          sb.append(")")
          sb.toString

        case False() => "false"
        case True() => "true"
        case Self() => "self"

        case intLit: IntLit => intLit.value.toString
        case strLit: StringLit => "\"" + strLit.value + "\""

        case id: Identifier =>
          sb.append(id.value)
          sb.append(getSymbolStr(id.getSymbol))
          sb.toString

        case newIntArr: NewIntArray =>
          "new Int[" +
          processExprStr(newIntArr.size, initialIndentCount) +
          "]"

        case newStmt: New =>
          "new " + newStmt.tpe.value + "()"

        case not: Not =>
          "!(" + processExprStr(not.expr, initialIndentCount)+")"

        // statements
        case block: Block =>
          sb.append("{\n")
          for (expr <- block.exprs) {
            sb.append(getNumIndents(initialIndentCount + 1))
            sb.append(processExprStr(expr, initialIndentCount + 1))
            sb.append(";")
            sb.append("\n")
          }
          if (sb.length > 2) {
            sb.setLength(sb.length-2)
          }
          sb.append(getNumIndents(initialIndentCount) + "}\n")
          sb.toString

        case ifStmt: If =>
          sb.append("if (")
          sb.append(processExprStr(ifStmt.cond, initialIndentCount) + ")\n")
          sb.append(getNumIndents(initialIndentCount + 1))
          sb.append(processExprStr(ifStmt.thn, initialIndentCount + 1) + "\n")

          if (ifStmt.els.isDefined) {
            sb.append(getNumIndents(initialIndentCount))
            sb.append("else \n ")
            sb.append(getNumIndents(initialIndentCount + 1))
            sb.append(processExprStr(ifStmt.els.get, initialIndentCount + 1))
          }
          sb.toString

        case whileStmt: While =>
          sb.append("while (")
          sb.append(processExprStr(whileStmt.cond, initialIndentCount) + ") ")
          sb.append(" " + processExprStr(whileStmt.body, 1) + "\n")
          sb.toString

        case println: Println =>
          "println(" + processExprStr(println.expr, initialIndentCount) + ")"

        case assign: Assign =>
          sb.append(assign.id.value)
          sb.append(getSymbolStr(assign.id.getSymbol))
          sb.append(" = ")
          sb.append(processExprStr(assign.expr, initialIndentCount))
          sb.toString

        case arrAssign: ArrayAssign =>
          arrAssign.id.value + "[" + processExprStr(arrAssign.index, initialIndentCount) + "] = " +
            processExprStr(arrAssign.expr, initialIndentCount)

        case strOf: StrOf =>
          "StrOf(" + processExprStr(strOf.expr, initialIndentCount) + ")"
        case _ =>
          "Type Undefined."
      }
    }

    def processVarStr(varDecl: VarDecl): String = {
      val sb = new StringBuilder()

      sb.append("var " + varDecl.getName)
      sb.append(getSymbolStr(varDecl.getSymbol))
      sb.append(": " + processTypeStr(varDecl.tpe))
      sb.toString()
    }

    def processFormalStr(formals : List[Formal]) : String = {
      val sb = new StringBuilder()

      for (formalItr <- formals) {
        sb.append(formalItr.getName)
        sb.append(getSymbolStr(formalItr.getSymbol))
        sb.append(": " + processTypeStr(formalItr.tpe))
        sb.append(", ")
      }

      if (sb.length > 2) {
        sb.setLength(sb.length-2) // discard the last two chars
      }
      sb.toString
    }

    def processTypeStr(tpe: TypeTree): String = tpe match {
        case IntType() => "Int"
        case StringType() => "String"
        case BooleanType() => "Bool"
        case IntArrayType() => "Int[]"
        case UnitType() => "Unit"
        case Identifier(_) => tpe.asInstanceOf[Identifier].value
        case udt: UserDefinedType =>
          udt.name + getSymbolStr(udt.getSymbol)
        case _ => "Type Undefined."
    }

    def getNumIndents(numIndents : Int) : String = {
      defaultIndent * numIndents
    }

    t match {
      case Program(_,_) =>
        val sb: StringBuilder = new StringBuilder()

        val prog = t.asInstanceOf[Program]
        val classList = prog.classes
        classList.map(classDecl => sb.append(processClassStr(classDecl)))

        val mainMethod = prog.main
        sb.append(processMethodStr(mainMethod.main))

        sb.toString
      case _ => "An instance of Program is expected."
    }
  } // end of method apply
} // end of object Printer

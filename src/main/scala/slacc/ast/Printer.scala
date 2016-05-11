package slacc
package ast

import Trees._
import utils._

object Printer {
  def apply(ctx: Context)(t: Tree): String = {
    val defaultIndent = "  "
    val numOfFirstLevelIndents = 1

    def processClassStr(classDecl: ClassDecl) : String = {
      val sb: StringBuilder = new StringBuilder()

      sb.append("class ")
      sb.append(classDecl.id.value)
      if (ctx.doSymbolIds) {
        sb.append("#" + classDecl.getSymbol.id + " ")
      }
      if (classDecl.parent.isDefined) {
        val parent = classDecl.parent
        sb.append("<: " + parent.get.value)
        if (ctx.doSymbolIds) {
          sb.append("#" + parent.get.getSymbol.id + " ")
        }
      }
      sb.append("{\n")


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
      if (ctx.doSymbolIds) {
        sb.append("#" + methodDecl.getSymbol.id + "(")
      }
      sb.append(processFormalStr(args) + ") : " + processTypeStr(retType) +" = {\n")

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

    def processExprStr(exprDecl: ExprTree, initialIndentCount: Int) : String = exprDecl match {
        case And(_,_) =>
          "(" + processExprStr(exprDecl.asInstanceOf[And].lhs, initialIndentCount) +
          " && " + processExprStr(exprDecl.asInstanceOf[And].rhs, initialIndentCount) + ")"

        case Or(_,_) =>
          "(" + processExprStr(exprDecl.asInstanceOf[Or].lhs, initialIndentCount) +
          " || " + processExprStr(exprDecl.asInstanceOf[Or].rhs, initialIndentCount) + ")"

        case Plus(_,_) =>
          "(" + processExprStr(exprDecl.asInstanceOf[Plus].lhs, initialIndentCount) +
          " + " + processExprStr(exprDecl.asInstanceOf[Plus].rhs, initialIndentCount) + ")"

        case Minus(_,_) =>
          "(" + processExprStr(exprDecl.asInstanceOf[Minus].lhs, initialIndentCount) +
          " - " + processExprStr(exprDecl.asInstanceOf[Minus].rhs, initialIndentCount) + ")"

        case Times(_,_) =>
          "(" + processExprStr(exprDecl.asInstanceOf[Times].lhs, initialIndentCount) +
          " * " + processExprStr(exprDecl.asInstanceOf[Times].rhs, initialIndentCount) + ")"

        case Div(_,_) =>
          "(" + processExprStr(exprDecl.asInstanceOf[Div].lhs, initialIndentCount) +
          " / " + processExprStr(exprDecl.asInstanceOf[Div].rhs, initialIndentCount) + ")"

        case LessThan(_,_) =>
          processExprStr(exprDecl.asInstanceOf[LessThan].lhs, initialIndentCount) +
          " < " + processExprStr(exprDecl.asInstanceOf[LessThan].rhs, initialIndentCount)

        case Equals(_,_) =>
          processExprStr(exprDecl.asInstanceOf[Equals].lhs, initialIndentCount) +
          " == " + processExprStr(exprDecl.asInstanceOf[Equals].rhs, initialIndentCount)

        case ArrayRead(_,_) =>
          processExprStr(exprDecl.asInstanceOf[ArrayRead].arr, initialIndentCount) +
          " [" + processExprStr(exprDecl.asInstanceOf[ArrayRead].index, initialIndentCount) + "] "

        case ArrayLength(_) =>
          processExprStr(exprDecl.asInstanceOf[ArrayLength].arr, initialIndentCount) + ".length"

        case MethodCall(_,_,_) =>
          val sb: StringBuilder = new StringBuilder

          sb.append(processExprStr(exprDecl.asInstanceOf[MethodCall].obj, initialIndentCount))
          sb.append(".")
          sb.append(processExprStr(exprDecl.asInstanceOf[MethodCall].meth, initialIndentCount))
          sb.append("(")

          val params = exprDecl.asInstanceOf[MethodCall].args
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

        case IntLit(_) => exprDecl.asInstanceOf[IntLit].value.toString
        case StringLit(_) => "\"" + exprDecl.asInstanceOf[StringLit].value + "\""

        case Identifier(_) =>
          val identifier = exprDecl.asInstanceOf[Identifier]
          identifier.value

        case NewIntArray(_) => "new Int[" +
          processExprStr(exprDecl.asInstanceOf[NewIntArray].size, initialIndentCount) + "]"

        case New(_) =>
          "new " + exprDecl.asInstanceOf[New].tpe.value + "()"

        case Not(_) =>
          "!(" + processExprStr(exprDecl.asInstanceOf[Not].expr, initialIndentCount)+")"

        // statements
        case Block(_) =>
          val sb: StringBuilder = new StringBuilder
          sb.append("{\n")

          var exprs = exprDecl.asInstanceOf[Block].exprs
          for (i <- 0 until exprs.length) {
            sb.append(getNumIndents(initialIndentCount + 1) +
              processExprStr(exprs(i), initialIndentCount + 1))
            if (i < exprs.length - 1)
              sb.append(";")
            sb.append("\n")
          }
          sb.append(getNumIndents(initialIndentCount) + "}\n")
          sb.toString

        case If(_, _, _) =>
          val sb: StringBuilder = new StringBuilder()
          sb.append("if (" +
            processExprStr(exprDecl.asInstanceOf[If].expr, initialIndentCount) + ")\n")
          sb.append(getNumIndents(initialIndentCount + 1) +
            processExprStr(exprDecl.asInstanceOf[If].thn, initialIndentCount + 1) + "\n")
          if (exprDecl.asInstanceOf[If].els.isDefined) {
            sb.append(getNumIndents(initialIndentCount) +
              "else \n " + getNumIndents(initialIndentCount + 1) +
              processExprStr(exprDecl.asInstanceOf[If].els.get, initialIndentCount + 1))
          }
          sb.toString

        case While(_,_) =>
          val sb: StringBuilder = new StringBuilder()
          sb.append("while (" +
            processExprStr(exprDecl.asInstanceOf[While].cond, initialIndentCount) + ")")
          sb.append(" " + processExprStr(exprDecl.asInstanceOf[While].body, 1) + "\n")
          sb.toString

        case Println(_) =>
          "println(" +
          processExprStr(exprDecl.asInstanceOf[Println].expr, initialIndentCount) + ")"

        case Assign(_,_) =>
          exprDecl.asInstanceOf[Assign].id.value + " = " +
          processExprStr(exprDecl.asInstanceOf[Assign].expr, initialIndentCount)

        case ArrayAssign(_,_,_) =>
          val arr = exprDecl.asInstanceOf[ArrayAssign]
          arr.id.value + "[" + processExprStr(arr.index, initialIndentCount) + "] = " +
            processExprStr(arr.expr, initialIndentCount)

        case Strof(_) =>
          "strOf(" + processExprStr(exprDecl.asInstanceOf[Strof].expr, initialIndentCount) + ")"
        case _ =>
          "Type Undefined."
    }

    def processVarStr(varDecl: VarDecl): String = {
      val sb = new StringBuilder()
      sb.append("var " + varDecl.id.value)

      if (ctx.doSymbolIds) {
        sb.append("#" + varDecl.getSymbol.id)
      }

      sb.append(": " + processTypeStr(varDecl.tpe))
      sb.toString()
    }

    def processFormalStr(formals : List[Formal]) : String = {
      val sb: StringBuilder = new StringBuilder()

      for (formalItr <- formals) {
        sb.append(formalItr.id.value)

        if (ctx.doSymbolIds) {
          sb.append("#" + formalItr.getSymbol.id)
        }

        sb.append(": " + processTypeStr(formalItr.tpe))
        sb.append(", ")
      }
      sb.setLength(sb.length-2) // discard the last two chars
      sb.toString
    }

    def processTypeStr(tpe: TypeTree): String = tpe match {
        case IntType() => "Int"
        case StringType() => "String"
        case BooleanType() => "Bool"
        case IntArrayType() => "Int[]"
        case UnitType() => "Unit"
        case Identifier(_) => tpe.asInstanceOf[Identifier].value
        case UserDefinedType(_) => tpe.asInstanceOf[UserDefinedType].id.value
        case _ => "Type Undefined."
    }

    def getNumIndents(numIndents : Int) : String = {
      defaultIndent * numIndents
    }

    t match {
      case Program(_, _) =>
        val prog = t.asInstanceOf[Program]
        val sb: StringBuilder = new StringBuilder()

        val classList = prog.classes
        classList.map(classDecl => sb.append(processClassStr(classDecl)))

        val mainMethod = prog.main
        sb.append(processMethodStr(mainMethod.main))

        sb.toString
      case _ => "An instance of Program is expected."
    }
  } // end of method apply
} // end of object Printer

package slacc
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {

    if (t.isInstanceOf[Program]) {
      val prog = t.asInstanceOf[Program]
      val sb: StringBuilder = new StringBuilder()

      val classList = prog.classes
      classList.map(classDecl => sb.append(processClassStr(classDecl)))

      val mainMethod = prog.main
      sb.append(processMethodStr(mainMethod.main))

      sb.toString
    }
    else {
      "An instance of Program is expected."
    }
  }

  def processClassStr(classDecl: ClassDecl) : String = {
    val sb: StringBuilder = new StringBuilder()

    var tabCount : Int = 0
    sb.append("class ")
    sb.append(classDecl.id.value + " ")
    if (classDecl.parent.isDefined) {
      sb.append("<: " + classDecl.parent.get.value + " ")
    }
    sb.append("{\n")
    tabCount += 1

    for (varItr <- classDecl.vars) {
      sb.append(getNumTabs(tabCount))
      sb.append(processVarStr(varItr))
      sb.append(";")
      sb.append("\n")
    }

    for (methodItr <- classDecl.methods) {
      sb.append(processMethodStr(methodItr, tabCount))
      sb.append("\n")
    }
    sb.append("}\n\n")
    sb.toString
  }



  def processMethodStr(methodDecl : MethodDecl, tabCount: Int = 0) : String = {
    val sb: StringBuilder = new StringBuilder()

    val id = methodDecl.id.value
    val args = methodDecl.args
    val vars = methodDecl.vars
    val exprs = methodDecl.exprs
    val retType = methodDecl.retType

    sb.append(getNumTabs(tabCount))
    sb.append("method " + id + "(")
    sb.append(processFormalStr(args) + ") : " + processTypeStr(retType) +" = {\n")

    for (varItr <- vars) {
      sb.append(getNumTabs(tabCount))
      sb.append(processVarStr(varItr))
      sb.append(";\n")
    }

    for (expr <- exprs) {
      sb.append(getNumTabs(tabCount + 1) + processExprStr(expr, tabCount + 1))
      sb.append(";\n")
    }

    sb.toString()
  }

  /*
   * Need tab count for multiline expressions, like if or while
   */
  def processExprStr(exprDecl : ExprTree, tabCount : Int) : String = exprDecl match {
      case And(_, _) => "(" + processExprStr(exprDecl.asInstanceOf[And].lhs, tabCount) +
        " && " + processExprStr(exprDecl.asInstanceOf[And].rhs, tabCount) + ")"
      case Or(_, _) => "(" + processExprStr(exprDecl.asInstanceOf[Or].lhs, tabCount) +
        " || " + processExprStr(exprDecl.asInstanceOf[Or].rhs, tabCount) + ")"
      case Plus(_, _) => "(" + processExprStr(exprDecl.asInstanceOf[Plus].lhs, tabCount) +
        " + " + processExprStr(exprDecl.asInstanceOf[Plus].rhs, tabCount) + ")"
      case Minus(_, _) => "(" + processExprStr(exprDecl.asInstanceOf[Minus].lhs, tabCount) +
        " - " + processExprStr(exprDecl.asInstanceOf[Minus].rhs, tabCount) + ")"
      case Times(_, _) => "(" + processExprStr(exprDecl.asInstanceOf[Times].lhs, tabCount) +
        " * " + processExprStr(exprDecl.asInstanceOf[Times].rhs, tabCount) + ")"
      case Div(_, _) => "(" + processExprStr(exprDecl.asInstanceOf[Div].lhs, tabCount) +
        " / " + processExprStr(exprDecl.asInstanceOf[Div].rhs, tabCount) + ")"
      case LessThan(_, _) => processExprStr(exprDecl.asInstanceOf[LessThan].lhs, tabCount) +
        " < " + processExprStr(exprDecl.asInstanceOf[LessThan].rhs, tabCount)
      case Equals(_,_) => processExprStr(exprDecl.asInstanceOf[Equals].lhs, tabCount) +
        " == " + processExprStr(exprDecl.asInstanceOf[Equals].rhs, tabCount)
      case ArrayRead(_,_) => processExprStr(exprDecl.asInstanceOf[ArrayRead].arr, tabCount) +
        " [" + processExprStr(exprDecl.asInstanceOf[ArrayRead].index, tabCount) + "] "
      case ArrayLength(_) => processExprStr(exprDecl.asInstanceOf[ArrayLength].arr, tabCount) +
        ".length"

      case MethodCall(_,_,_) => {
        val sb: StringBuilder = new StringBuilder

        sb.append(processExprStr(exprDecl.asInstanceOf[MethodCall].obj, tabCount))
        sb.append(".")
        sb.append(processExprStr(exprDecl.asInstanceOf[MethodCall].meth, tabCount))
        sb.append("(")

        val params = exprDecl.asInstanceOf[MethodCall].args
        for (param <- params) {
          sb.append(processExprStr(param, tabCount + 1))
          sb.append(", ")
        }
        sb.setLength(sb.length-2)
        sb.append(")")
        sb.toString
      }

      case False() => "false"
      case True() => "true"
      case Self() => "self"

      case IntLit(_) => exprDecl.asInstanceOf[IntLit].value.toString
      case StringLit(_) => "\"" + exprDecl.asInstanceOf[StringLit].value + "\""

      case Identifier(_) => {
        val identifier = exprDecl.asInstanceOf[Identifier]
        // identifier.value + "#" + identifier.getSymbol.id
        identifier.value
      }
      case NewIntArray(_) => "new Int[" +
        processExprStr(exprDecl.asInstanceOf[NewIntArray].size, tabCount) + "]"
      case New(_) => "new " + exprDecl.asInstanceOf[New].tpe.value + "()"
      case Not(_) => "!(" + processExprStr(exprDecl.asInstanceOf[Not].expr, tabCount)+")"

      // statements
      case Block(_) => {
        val sb: StringBuilder = new StringBuilder
        sb.append("{\n")

        var exprs = exprDecl.asInstanceOf[Block].exprs
        for (i <- 0 until exprs.length) {
          sb.append(getNumTabs(tabCount + 1) + processExprStr(exprs(i), tabCount + 1))
          if (i < exprs.length - 1)
            sb.append(";")
          sb.append("\n")
        }
        sb.append(getNumTabs(tabCount) + "}\n")
        sb.toString
      }
      case If(_, _, _) => {
        val sb: StringBuilder = new StringBuilder()
        sb.append("if (" + processExprStr(exprDecl.asInstanceOf[If].expr, tabCount) + ")\n")
        sb.append(getNumTabs(tabCount + 1) +
          processExprStr(exprDecl.asInstanceOf[If].thn, tabCount + 1) + "\n")
        if (exprDecl.asInstanceOf[If].els.isDefined) {
          sb.append(getNumTabs(tabCount) + "else \n " + getNumTabs(tabCount + 1) +
            processExprStr(exprDecl.asInstanceOf[If].els.get, tabCount + 1))
        }
        sb.toString
      }
      case While(_,_) => {
        val sb: StringBuilder = new StringBuilder()
        sb.append("while (" + processExprStr(exprDecl.asInstanceOf[While].cond, tabCount) + ")")
        sb.append(" " + processExprStr(exprDecl.asInstanceOf[While].body, 1) + "\n")
        sb.toString
      }
      case Println(_) => "println(" + processExprStr(exprDecl.asInstanceOf[Println].expr, tabCount) + ")"
      case Assign(_,_) => exprDecl.asInstanceOf[Assign].id.value + " = " +
        processExprStr(exprDecl.asInstanceOf[Assign].expr, tabCount)
      case ArrayAssign(_,_,_) => {
        val arr = exprDecl.asInstanceOf[ArrayAssign]
        arr.id.value + "[" + processExprStr(arr.index, tabCount) + "] = " +
          processExprStr(arr.expr, tabCount)
      }
      case Strof(_) => "strOf(" + processExprStr(exprDecl.asInstanceOf[Strof].expr, tabCount) + ")"
      case _ => "NoSuchType"
  }

  def processVarStr(varDecl: VarDecl): String = {
    "var " + varDecl.id.value + "#" + varDecl.getSymbol.id + " : " + processTypeStr(varDecl.tpe)
  }

  def processFormalStr(formals : List[Formal]) : String = {
    val sb: StringBuilder = new StringBuilder()

    for (formalItr <- formals) {
      sb.append(formalItr.id.value + " : " + processTypeStr(formalItr.tpe))
      sb.append(", ")
    }
    sb.setLength(sb.length-2) // discard the last two chars
    sb.toString
  }

  def getNumTabs(numTabs : Int) : String = {
    val sb: StringBuilder = new StringBuilder()
    for (i <- 0 until numTabs) {
      sb.append("\t")
    }
    sb.toString()
  }

  def processTypeStr(tpe: TypeTree): String = tpe match {
      case IntType() => "Int"
      case StringType() => "String"
      case BooleanType() => "Bool"
      case IntArrayType() => "Int[]"
      case UnitType() => "Unit"
      case Identifier(_) => tpe.asInstanceOf[Identifier].value
      case UserDefinedType(_) => tpe.asInstanceOf[UserDefinedType].identifier.value
      case _ => "NoSuchType"
  }
}

package slacc
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
    var sb: StringBuilder = new StringBuilder()
    var tabCounter = 0
    val classList = t.asInstanceOf[Program].classes
    val mainMethod = t.asInstanceOf[Program].main
    classList.map(clazz => {
      sb.append(getClassString(clazz))
    })
    sb.append(getMethodString(mainMethod.main, tabCounter))
    sb.toString
  }

  def getClassString(classDecl : ClassDecl) : String = {
    var sb: StringBuilder = new StringBuilder()
    var tabCount : Int = 0
    sb.append("class ")
    sb.append(classDecl.id.value + " ")
    if (classDecl.parent.isDefined) {
      sb.append("<: " + classDecl.parent.get.value + " ")
    }
    sb.append("{\n")
    tabCount += 1

    for (i <- 0 until classDecl.vars.length) {
      val varDecl = classDecl.vars(i)
      sb.append(getNumTabs(tabCount))
      sb.append(getVarString(varDecl))
      sb.append(";")
      sb.append("\n")
    }

    for (i <- 0 until classDecl.methods.length) {
      var methodDecl = classDecl.methods(i)
      // methods handle their indentation
      sb.append(getMethodString(methodDecl, tabCount))
      sb.append("\n")
    }
    sb.append("}\n\n")
    sb.toString
  }

  def getVarString(varDecl : VarDecl) : String = {
    "var " + varDecl.id.value + " : " + getTypeString(varDecl.tpe)
  }

  def getFormalString(formals : List[Formal]) : String = {
    var sb:StringBuilder = new StringBuilder()
    for (i <- 0 until formals.length) {
      var formalDecl : Formal = formals(i)
      sb.append(formalDecl.id.value + " : " + getTypeString(formalDecl.tpe))
      if (i < formals.length - 1)
          sb.append(", ")
    }
    sb.toString
  }

  def getNumTabs(numTabs : Int) : String = {
    var sb:StringBuilder = new StringBuilder()
      for (i <- 0 until numTabs) {
        sb.append("\t")
      }
    sb.toString()
  }

  def getMethodString(methodDecl : MethodDecl, tabCount : Int) : String = {
    val sb: StringBuilder = new StringBuilder()

    val id = methodDecl.id.value
    val args = methodDecl.args
    val vars = methodDecl.vars
    val exprs = methodDecl.exprs
    val retExpr = methodDecl.retExpr
    val retType = methodDecl.retType

    sb.append(getNumTabs(tabCount))
    sb.append("method " + id + "(")
    sb.append(getFormalString(args) + ") : " + getTypeString(retType) +" = {\n")

    for (i <- 0 until vars.length) {
      val varDecl = vars(i)
      sb.append(getNumTabs(tabCount))
      sb.append(getVarString(varDecl))
      sb.append(";\n")
    }

    for (i <- 0 until exprs.length) {
      var exprDecl = exprs(i)
      sb.append(getNumTabs(tabCount + 1) + getExprString(exprDecl, tabCount + 1))
      sb.append(";\n")
    }
    sb.append(getNumTabs(tabCount + 1) + getExprString(retExpr, tabCount + 1) +
      "\n" + getNumTabs(tabCount) + "}\n")
    sb.toString()
  }

  /*
   * Need tab count for multiline expressions, like if or while
   */
  def getExprString(exprDecl : ExprTree, tabCount : Int) : String = exprDecl match {
      case And(_,_) => "(" + getExprString(exprDecl.asInstanceOf[And].lhs, tabCount) +
        " && " + getExprString(exprDecl.asInstanceOf[And].rhs, tabCount) + ")"
      case Or(_,_) => "(" + getExprString(exprDecl.asInstanceOf[Or].lhs, tabCount) +
        " || " + getExprString(exprDecl.asInstanceOf[Or].rhs, tabCount) + ")"
      case Plus(_,_) => "(" + getExprString(exprDecl.asInstanceOf[Plus].lhs, tabCount) +
        " + " + getExprString(exprDecl.asInstanceOf[Plus].rhs, tabCount) + ")"
      case Minus(_,_) => "(" + getExprString(exprDecl.asInstanceOf[Minus].lhs, tabCount) +
        " - " + getExprString(exprDecl.asInstanceOf[Minus].rhs, tabCount) + ")"
      case Times(_,_) => "(" + getExprString(exprDecl.asInstanceOf[Times].lhs, tabCount) +
        " * " + getExprString(exprDecl.asInstanceOf[Times].rhs, tabCount) + ")"
      case Div(_,_) => "(" + getExprString(exprDecl.asInstanceOf[Div].lhs, tabCount) +
        " / " + getExprString(exprDecl.asInstanceOf[Div].rhs, tabCount) + ")"
      case LessThan(_,_) => getExprString(exprDecl.asInstanceOf[LessThan].lhs, tabCount) +
        " < " + getExprString(exprDecl.asInstanceOf[LessThan].rhs, tabCount)
      case Equals(_,_) => getExprString(exprDecl.asInstanceOf[Equals].lhs, tabCount) +
        " == " + getExprString(exprDecl.asInstanceOf[Equals].rhs, tabCount)
      case ArrayRead(_,_) => getExprString(exprDecl.asInstanceOf[ArrayRead].arr, tabCount) +
        " [" + getExprString(exprDecl.asInstanceOf[ArrayRead].index, tabCount) + "] "
      case ArrayLength(_) => getExprString(exprDecl.asInstanceOf[ArrayLength].arr, tabCount) +
        ".length"
      case MethodCall(_,_,_) => {
        var methodNoParams = getExprString(exprDecl.asInstanceOf[MethodCall].obj, tabCount) +
          "." + getExprString(exprDecl.asInstanceOf[MethodCall].meth, tabCount) + "("
        val params = exprDecl.asInstanceOf[MethodCall].args

        for (i <- 0 until params.length) {
          var paramDecl = params(i)
          methodNoParams = methodNoParams + getExprString(paramDecl, tabCount + 1)
          if (i < params.length - 1)
              methodNoParams = methodNoParams + ", "
        }
        methodNoParams + ")"
      }
      case IntLit(_) => "" + exprDecl.asInstanceOf[IntLit].value
      case StringLit(_) => "\"" + exprDecl.asInstanceOf[StringLit].value + "\""
      case True() => "true"
      case False() => "false"
      case Identifier(_) => exprDecl.asInstanceOf[Identifier].value
      case Self() => "self"
      case NewIntArray(_) => "new Int[" +
        getExprString(exprDecl.asInstanceOf[NewIntArray].size, tabCount) + "]"
      case New(_) => "new " + exprDecl.asInstanceOf[New].tpe.value + "()"
      case Not(_) => "!(" + getExprString(exprDecl.asInstanceOf[Not].expr, tabCount)+")"
      // statements
      case Block(_) => {
        var sb:StringBuilder = new StringBuilder()
        sb.append("{\n")
        var exprs = exprDecl.asInstanceOf[Block].exprs
        for (i <- 0 until exprs.length) {
          sb.append(getNumTabs(tabCount + 1) + getExprString(exprs(i), tabCount + 1))
          if (i < exprs.length - 1)
            sb.append(";")
          sb.append("\n")
        }
        sb.append(getNumTabs(tabCount) + "}\n")
        sb.toString
      }
      case If(_,_,_) => {
        var sb:StringBuilder = new StringBuilder()
        sb.append("if (" + getExprString(exprDecl.asInstanceOf[If].expr, tabCount) + ")\n")
        sb.append(getNumTabs(tabCount + 1) +
          getExprString(exprDecl.asInstanceOf[If].thn, tabCount + 1) + "\n")
        if (exprDecl.asInstanceOf[If].els.isDefined) {
          sb.append(getNumTabs(tabCount) + "else \n " + getNumTabs(tabCount + 1) +
            getExprString(exprDecl.asInstanceOf[If].els.get, tabCount + 1))
        }
        sb.toString
      }
      case While(_,_) => {
        var sb:StringBuilder = new StringBuilder()
        sb.append("while (" + getExprString(exprDecl.asInstanceOf[While].cond, tabCount) + ")")
        sb.append(getNumTabs(tabCount + 1) +
          getExprString(exprDecl.asInstanceOf[While].body, tabCount + 1) + "\n")
        sb.toString
      }
      case Println(_) => "println(" + getExprString(exprDecl.asInstanceOf[Println].expr, tabCount) + ")"
      case Assign(_,_) => exprDecl.asInstanceOf[Assign].id.value + " = " +
        getExprString(exprDecl.asInstanceOf[Assign].expr, tabCount)
      case ArrayAssign(_,_,_) => {
        val arr = exprDecl.asInstanceOf[ArrayAssign]
        arr.id.value + "[" + getExprString(arr.index, tabCount) + "] = " +
          getExprString(arr.expr, tabCount)
      }
      case Strof(_) => "strOf(" + getExprString(exprDecl.asInstanceOf[Strof].expr, tabCount) + ")"
      case _ => "NoSuchType"
  }

  def getTypeString(tpe: TypeTree): String = tpe match {
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

package slacc
package analyzer

import slacc.analyzer.Symbols.{ClassSymbol, GlobalScope, MethodSymbol, VariableSymbol}
import slacc.ast.Printer
import slacc.ast.Trees._
import slacc.utils._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {

    val gs = GlobalScope()

    println("Starting Name Analysis:")

    // Step 1: Collect symbols in declarations


    for(classDecl <- prog.classes) {
      gs.classes + (classDecl.id.value -> classDecl)
      val classSymbol = classDecl.asInstanceOf[ClassSymbol]

      for (memberDecl <- classDecl.vars) {
        classSymbol.members + (memberDecl.id.value -> memberDecl)
      }

      for (methodDecl <- classDecl.methods) {
        classSymbol.methods + (methodDecl.id.value -> methodDecl)
        val methodSymbol = methodDecl.asInstanceOf[MethodSymbol]

        for (argDecl <- methodDecl.args) {
          methodSymbol.params

        }

        for (varDecl <- methodDecl.vars) {
          methodSymbol.members
        }

        methodSymbol.argList = List[VariableSymbol]()

      }
    }

    val mainClassDecl = prog.main
    val mainMethodDecl = mainClassDecl.main
    val mainMethodSymbol = mainMethodDecl.asInstanceOf[MethodSymbol]

    for (argDecl <- mainMethodDecl.args) {
      mainMethodSymbol.params

    }

    for (varDecl <- mainMethodDecl.vars) {
      mainMethodSymbol.members
    }

    mainMethodSymbol.argList = List[VariableSymbol]()



    // Step 2: Attach symbols to identifiers (except method calls) in method bodies

    // wrap the main method into Main class
    mainClassDecl.setSymbol(new ClassSymbol("Main"))
    mainMethodDecl.setSymbol(new MethodSymbol(mainMethodDecl.id.value, mainClassDecl.getSymbol))

    for (varDecl <- mainMethodDecl.vars) {
      varDecl.id.setSymbol(new VariableSymbol(varDecl.id.value))
    }
    for (expr <- mainMethodDecl.exprs) {
      if (expr.isInstanceOf[Identifier]) {

      }
    }

    for (expr <- prog.main.exprs) {
      if (expr.isInstanceOf[Identifier]) {
        val identifier = expr.asInstanceOf[Identifier]
        identifier.setSymbol(new VariableSymbol(identifier.value))
      }

    }

    // (Step 3:) Print tree with symbol ids for debugging
    println(Printer(prog))

    // Make sure you check all constraints

    prog
  }
}

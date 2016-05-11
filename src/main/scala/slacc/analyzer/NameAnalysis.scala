package slacc
package analyzer

import slacc.analyzer.Symbols.{ClassSymbol, GlobalScope, MethodSymbol, VariableSymbol}
import slacc.ast.Printer
import slacc.ast.Trees._
import slacc.utils._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {

    def collectMethodSymbol(methodDecl: MethodDecl, classSymbol: ClassSymbol): MethodSymbol = {
      val methodSymbol = new MethodSymbol(methodDecl.id.value, classSymbol)

      // parameters
      var argList = List[VariableSymbol]()
      for (argDecl <- methodDecl.args) {
        val paramSymbol = new VariableSymbol(argDecl.id.value)
        methodSymbol.params += (paramSymbol.name -> paramSymbol)
        argList :+= paramSymbol
      }
      // arg list
      methodSymbol.argList = argList

      // member variables
      for (varDecl <- methodDecl.vars) {
        val memberSymbol = new VariableSymbol(varDecl.id.value)
        methodSymbol.members += (memberSymbol.name -> memberSymbol)
      }

      // overridden

      // finished, add to class
      classSymbol.methods += (methodSymbol.name -> methodSymbol)

      methodSymbol
    }

    val gs = GlobalScope()

    println("Starting Name Analysis:")

    // Step 1: Collect symbols in declarations
    for (classDecl <- prog.classes) {
      val classSymbol = new ClassSymbol(classDecl.id.value)

      // add members
      for (memberDecl <- classDecl.vars) {
        val memberSymbol = new VariableSymbol(memberDecl.id.value)
        classSymbol.members += (memberSymbol.name -> memberSymbol)
      }

      // add methods
      for (methodDecl <- classDecl.methods) {
        collectMethodSymbol(methodDecl, classSymbol)
      }

      // add class to the map
      gs.classes += (classDecl.id.value -> classSymbol)
    }

    // set parents if any
    for (classDecl <- prog.classes) {
      if (classDecl.parent.isDefined) {
        val className = classDecl.id.value
        val parentName = classDecl.parent.get.value

        val classSymbol = gs.classes get className
        val classParentSymbol = gs.classes get parentName

        if (classParentSymbol.isDefined) {
          classSymbol.get.parent = classParentSymbol
        }
        else {
          // report error
          println("Warning: A parent, " +  parentName + ",  of class, " + className + ", is not define.")
        }
      }
    }

    val mainClassDecl = prog.main
    val mainMethodDecl = mainClassDecl.main

    val mainClassSymbol = new ClassSymbol(mainClassDecl.id.value)
    val mainMethodSymbol = collectMethodSymbol(mainMethodDecl, mainClassSymbol)

    gs.mainClass = mainClassSymbol


    // Step 2: Attach symbols to identifiers (except method calls) in method bodies

    def setMethodSymbols(methodDecl: MethodDecl, methodSymbol: MethodSymbol): Unit = {

      for (argDecl <- methodDecl.args) {
        val argName = argDecl.id.value
        val argSymbol = (methodSymbol.params get argName).get

        argDecl.setSymbol(argSymbol)
      }

      for (varDecl <- methodDecl.vars) {
        val varName = varDecl.id.value
        val varSymbol = (methodSymbol.members get varName).get

        varDecl.setSymbol(varSymbol)
      }

      methodDecl.setSymbol(methodSymbol)
    }

    for (classDecl <- prog.classes) {
      val className = classDecl.id.value
      val classSymbol = (gs.classes get className).get

      if (classDecl.parent.isDefined) {
        val parentName = classDecl.parent.get.value
        val parentSymbol = gs.classes get parentName

        classDecl.parent.get.setSymbol(parentSymbol.get)
        classSymbol.parent = parentSymbol

      }

      for (methodDecl <- classDecl.methods) {
        val methodName = methodDecl.id.value
        val methodSymbol = (classSymbol.methods get methodName).get

        setMethodSymbols(methodDecl, methodSymbol)
      }

      classDecl.setSymbol(classSymbol)
    }

    mainMethodDecl.setSymbol(mainMethodSymbol)
    mainClassDecl.setSymbol(mainClassSymbol)
    setMethodSymbols(mainMethodDecl, mainMethodSymbol)


    // (Step 3:) Print tree with symbol ids for debugging
    println(Printer(ctx)(prog))

    // Make sure you check all constraints

    prog
  }
}

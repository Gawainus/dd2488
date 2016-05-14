package slacc
package analyzer

import slacc.analyzer.Symbols.{ClassSymbol, GlobalScope, MethodSymbol, VariableSymbol}
import slacc.analyzer.Types.TClass
import slacc.ast.Trees._
import slacc.utils._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def collectMethodSymbol(methodDecl: MethodDecl, classSymbol: ClassSymbol): MethodSymbol = {
      val methodSymbol = MethodSymbol(methodDecl.getName, classSymbol)

      methodSymbol.setPos(methodDecl)
      methodSymbol.setType(methodDecl.retType.getType)

      // parameters
      var argList = List[VariableSymbol]()
      for (argDecl <- methodDecl.args) {
        val paramSymbol = VariableSymbol(argDecl.getName)

        paramSymbol.setPos(argDecl)
        paramSymbol.setType(argDecl.tpe.getType)

        methodSymbol.params += (paramSymbol.name -> paramSymbol)
        argList :+= paramSymbol
      }

      // arg list
      methodSymbol.argList = argList

      // member variables
      for (varDecl <- methodDecl.vars) {
        val memberSymbol = VariableSymbol(varDecl.getName)
        memberSymbol.setPos(varDecl)
        memberSymbol.setType(varDecl.tpe.getType)

        methodSymbol.members += (memberSymbol.name -> memberSymbol)
      }

      // overridden

      // finished, add to class
      classSymbol.methods += (methodSymbol.name -> methodSymbol)
      methodSymbol
    }

    println("Starting Name Analysis:")
    val gs = GlobalScope()

    // Step 1: Collect symbols in declarations
    for (classDecl <- prog.classes) {
      val classSymbol = ClassSymbol(classDecl.getName)
      classSymbol.setPos(classDecl)
      classSymbol.setType(TClass(classDecl.getName))

      // add members
      for (memberDecl <- classDecl.vars) {
        val memberSymbol = VariableSymbol(memberDecl.getName)
        memberSymbol.setPos(memberDecl)
        memberSymbol.setType(memberDecl.tpe.getType)

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
        val className = classDecl.getName
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

    val mainClassSymbol = ClassSymbol(mainClassDecl.getName)
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

      def setMethodExprsSymbols(methodDecl: MethodDecl, methodSymbol: MethodSymbol): Unit = {
        def setExprSymbol(expr: ExprTree, methodSymbol: MethodSymbol): Unit = {
          def setVarSymbolIfIdentifier(expr: ExprTree, methodSymbol: MethodSymbol): Unit = {
            def getVarSymbol(name: String, methodSymbol: MethodSymbol): Option[VariableSymbol] = {
              val varSymbolFromParams = methodSymbol.params get name
              val varSymbolFromMembers = methodSymbol.members get name

              val classSymbol = methodSymbol.classSymbol
              val varSymbolFromClass = classSymbol.members get name

              if (varSymbolFromParams.isDefined) {
                varSymbolFromParams
              }
              else if (varSymbolFromMembers.isDefined) {
                varSymbolFromMembers
              }
              else if (varSymbolFromClass.isDefined) {
                varSymbolFromClass
              }
              else {
                var parent = classSymbol.parent
                var varSymbol: Option[VariableSymbol] = None

                while (parent.isDefined && varSymbol.isEmpty) {
                  varSymbol = parent.get.members get name
                  parent = parent.get.parent
                }
                varSymbol
              }
            }
            expr match {
              case id: Identifier =>
                val varSymbol = getVarSymbol(id.value, methodSymbol)
                if (varSymbol.isDefined) {
                  id.setSymbol(varSymbol.get)
                }
                else {
                  error("Undeclared identifier: " + id.value, id)

                }
              case _ =>
                setExprSymbol(expr, methodSymbol)
            }
          }

          def setMethodSymbolIfIdentifier(expr: ExprTree, classSymbol: ClassSymbol): Unit = {
            def getMethodSymbol(name: String, classSymbol: ClassSymbol): Option[MethodSymbol] = {
              val msFromClass = classSymbol.methods get name
              if (msFromClass.isDefined) {
                msFromClass
              }
              else {
                var parent = classSymbol.parent
                var msFromParents: Option[MethodSymbol] = None

                while (parent.isDefined && msFromParents.isEmpty) {
                  msFromParents = parent.get.methods get name
                  parent = parent.get.parent
                }
                msFromParents
              }
            }
            expr match {
              case id: Identifier =>
                val ms = getMethodSymbol(id.value, classSymbol)
                if (ms.isDefined) {
                  id.setSymbol(ms.get)
                }
                else {
                  error("Undeclared identifier: " + id.value, id)
                }
              case _ =>
                setExprSymbol(expr, methodSymbol)
            }
          }
          expr match {
            case and: And =>
              setVarSymbolIfIdentifier(and.lhs, methodSymbol)
              setVarSymbolIfIdentifier(and.rhs, methodSymbol)

            case or: Or =>
              setVarSymbolIfIdentifier(or.lhs, methodSymbol)
              setVarSymbolIfIdentifier(or.rhs, methodSymbol)

            case le: LessThan =>
              setVarSymbolIfIdentifier(le.lhs, methodSymbol)
              setVarSymbolIfIdentifier(le.rhs, methodSymbol)

            case eq: Equals =>
              setVarSymbolIfIdentifier(eq.lhs, methodSymbol)
              setVarSymbolIfIdentifier(eq.rhs, methodSymbol)

            case plus: Plus =>
              setVarSymbolIfIdentifier(plus.lhs, methodSymbol)
              setVarSymbolIfIdentifier(plus.rhs, methodSymbol)

            case minus: Minus =>
              setVarSymbolIfIdentifier(minus.lhs, methodSymbol)
              setVarSymbolIfIdentifier(minus.rhs, methodSymbol)

            case times: Times =>
              setVarSymbolIfIdentifier(times.lhs, methodSymbol)
              setVarSymbolIfIdentifier(times.rhs, methodSymbol)

            case div: Div =>
              setVarSymbolIfIdentifier(div.lhs, methodSymbol)
              setVarSymbolIfIdentifier(div.rhs, methodSymbol)

            case arrRead: ArrayRead =>
              setVarSymbolIfIdentifier(arrRead.arr, methodSymbol)
              setVarSymbolIfIdentifier(arrRead.index, methodSymbol)

            case arrLeng: ArrayLength =>
              setVarSymbolIfIdentifier(arrLeng.arr, methodSymbol)

            case methodCall: MethodCall =>
              setVarSymbolIfIdentifier(methodCall.obj, methodSymbol)

              for (arg <- methodCall.args) {
                setVarSymbolIfIdentifier(arg, methodSymbol)
              }

            case id: Identifier =>
              setVarSymbolIfIdentifier(id, methodSymbol)

            case not: Not =>
              setVarSymbolIfIdentifier(not.expr, methodSymbol)

            case block: Block =>
              for(expr <- block.exprs) {
                setVarSymbolIfIdentifier(expr, methodSymbol)
              }

            case ifStmt: If =>
              setVarSymbolIfIdentifier(ifStmt.cond, methodSymbol)
              setVarSymbolIfIdentifier(ifStmt.thn, methodSymbol)
              if (ifStmt.els.isDefined) {
                setVarSymbolIfIdentifier(ifStmt.els.get, methodSymbol)
              }

            case whileStmt: While =>
              setVarSymbolIfIdentifier(whileStmt.cond, methodSymbol)
              setVarSymbolIfIdentifier(whileStmt.body, methodSymbol)

            case println: Println =>
              setVarSymbolIfIdentifier(println.expr, methodSymbol)

            case assign: Assign =>
              setVarSymbolIfIdentifier(assign.id, methodSymbol)
              setVarSymbolIfIdentifier(assign.expr, methodSymbol)

            case arrAssign: ArrayAssign =>
              setVarSymbolIfIdentifier(arrAssign.id, methodSymbol)
              setVarSymbolIfIdentifier(arrAssign.index, methodSymbol)
              setVarSymbolIfIdentifier(arrAssign.expr, methodSymbol)

            case strOf: StrOf =>
              setVarSymbolIfIdentifier(strOf.expr, methodSymbol)


            case _ =>
          }
        }

        for (expr <- methodDecl.exprs) {
          setExprSymbol(expr, methodSymbol)
        }
      }

      setMethodExprsSymbols(methodDecl, methodSymbol)
      methodDecl.setSymbol(methodSymbol)
    }


    // set symbols for every class
    for (classDecl <- prog.classes) {
      val className = classDecl.id.value
      val classSymbol = (gs.classes get className).get

      if (classDecl.parent.isDefined) {
        val parentName = classDecl.parent.get.value
        val parentSymbol = gs.classes get parentName

        classDecl.parent.get.setSymbol(parentSymbol.get)
        classSymbol.parent = parentSymbol

      }

      for (varDecl <- classDecl.vars) {
        val varName = varDecl.getName
        val varSymbol = (classSymbol.members get varName).get

        varDecl.setSymbol(varSymbol)
      }

      for (methodDecl <- classDecl.methods) {
        val methodName = methodDecl.getName
        val methodSymbol = (classSymbol.methods get methodName).get

        setMethodSymbols(methodDecl, methodSymbol)
      }

      classDecl.setSymbol(classSymbol)
    }

    setMethodSymbols(mainMethodDecl, mainMethodSymbol)
    mainMethodDecl.setSymbol(mainMethodSymbol)
    mainClassDecl.setSymbol(mainClassSymbol)


    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints


    // report unaccessed variables
"    test.slac:18:5: Warning: Variable v1 is declared but never used. var v1: Int; ^"


    prog
  }
}

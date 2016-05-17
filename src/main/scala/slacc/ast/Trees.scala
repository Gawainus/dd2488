package slacc
package ast

import utils._
import analyzer.Symbols._
import analyzer.Types._

object Trees {
  sealed trait Tree extends Positioned

  sealed trait Identified {
    val id: Identifier
    def getName: String = {
      id.value
    }
  }

  case class Program(main: MainMethod, classes: List[ClassDecl]) extends Tree

  // Note: we attach a `ClassSymbol`,
  // because the main method should be put into a (synthetic) class called "Main";
  // the attached `ClassSymbol` is then the symbol of this "Main" class.
  case class MainMethod(main: MethodDecl) extends Tree with Identified with Symbolic[ClassSymbol] {
    val id = Identifier("Main")
    def exprs: List[ExprTree] = main.exprs ::: (main.retExpr :: Nil)
  }

  case class ClassDecl(id: Identifier,
                       parent: Option[Identifier],
                       vars: List[VarDecl],
                       methods: List[MethodDecl])
    extends Tree with Identified with Symbolic[ClassSymbol]


  case class MethodDecl(retType: TypeTree,
                        id: Identifier,
                        args: List[Formal],
                        vars: List[VarDecl],
                        exprs: List[ExprTree],
                        retExpr: ExprTree)
    extends Tree with Identified with Symbolic[MethodSymbol]

  case class VarDecl(tpe: TypeTree, id: Identifier) extends Tree
    with Identified with Symbolic[VariableSymbol]

  sealed case class Formal(tpe: TypeTree, id: Identifier) extends Tree
    with Identified with Symbolic[VariableSymbol]

  sealed trait TypeTree extends Tree with Typed
  case class IntType() extends TypeTree {setType(TInt)}
  case class LongType() extends TypeTree {setType(TLong)}
  case class IntArrayType() extends TypeTree {setType(TIntArray)}
  case class Double() extends TypeTree {setType(TDouble)}
  case class BooleanType() extends TypeTree {setType(TBoolean)}
  case class StringType() extends TypeTree {setType(TString)}
  case class UnitType() extends TypeTree {setType(TUnit)}

  case class UserDefinedType(name: String) extends TypeTree with Symbolic[ClassSymbol]


  sealed trait ExprTree extends Tree with Typed
  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree {setPos(lhs) setType TBoolean}
  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree {setPos(lhs) setType TBoolean}

  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree {setPos(lhs) setType TBoolean}
  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree {setPos(lhs) setType TBoolean}

  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree {setPos(lhs)}
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree {setPos(lhs)}
  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree {setPos(lhs)}
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree {setPos(lhs)}

  case class ArrayRead(arr: ExprTree, index: ExprTree) extends ExprTree {setPos(arr) setType(TInt)}
  case class ArrayLength(arr: ExprTree) extends ExprTree {setPos(arr) setType(TInt)}

  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree])
    extends ExprTree {setPos(obj)}

  case class IntLit(value: Int) extends ExprTree {setType(TInt)}
  case class StringLit(value: String) extends ExprTree {setType(TString)}

  case class True() extends ExprTree {setType(TBoolean)}
  case class False() extends ExprTree {setType(TBoolean)}

  case class Identifier(value: String) extends TypeTree with ExprTree with Symbolic[Symbol] {
    // The type of the identifier depends on the type of the symbol
    override def getType: Type = getSymbol match {
      case cs: ClassSymbol =>
        TClass(cs)

      case ms: MethodSymbol =>
        //sys.error("Requesting type of a method identifier.")
        ms.getType
      case vs: VariableSymbol =>
        vs.getType
    }
    override def setType(tpe: Type) = {
      sys.error("Type of an Identifier is overriden by its Symbol's type. You can only set its Symbol")
      this
    }
  } // end of class Identifer

  case class Self() extends ExprTree with Symbolic[ClassSymbol]

  case class NewIntArray(size: ExprTree) extends ExprTree {setType(TIntArray)}
  case class New(cls: Identifier) extends ExprTree

  case class Not(expr: ExprTree) extends ExprTree {setType(TBoolean)}

  case class Block(exprs: List[ExprTree]) extends ExprTree {setType(TUnit)}
  case class If(cond: ExprTree, thn: ExprTree, els: Option[ExprTree]) extends ExprTree {setType(TUnit)}
  case class While(cond: ExprTree, body: ExprTree) extends ExprTree {setType(TUnit)}
  case class Println(expr: ExprTree) extends ExprTree {setType(TUnit)}
  case class Assign(id: Identifier, expr: ExprTree)
    extends ExprTree {setPos(id)}
  case class ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree)
    extends ExprTree {setPos(id)}
  case class StrOf(expr: ExprTree) extends ExprTree {setType(TString)}
} // end of object Trees

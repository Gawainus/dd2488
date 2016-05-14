package slacc
package analyzer

import slacc.analyzer.Symbols._

object Types {
  trait Typed {
    self =>

    private var _tpe: Type = TUntyped

    def setType(tpe: Type): self.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }

  // TODO: Complete by creating necessary types
  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _ => false
    }
    override def toString = "Boolean"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _ => false
    }
    override def toString = "Int"
  }

  case object TIntArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TIntArray => true
      case _ => false
    }
    override def toString = "IntArray"
  }

  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _ => false
    }
    override def toString = "String"
  }

  case object TUnit extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TUnit => true
      case _ => false
    }
    override def toString = "Unit"
  }

  case class TClass(name: String) extends Type {

    override def isSubTypeOf(tpe: Type): Boolean = {
      false
    }

    def isSubClassOf(parentName: String): Boolean = {

      false
    }

    override def toString = "TClass " + name
  }


  // Todo: fix this
  case class TObject(classSymbol: ClassSymbol) extends Type {
    val className = classSymbol.name

    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TObject(tpeSymbol) =>

        val testName = tpeSymbol.name


        true
      case _ =>
        false
    }

    override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  val anyObject = TObject(new ClassSymbol("Object"))
}

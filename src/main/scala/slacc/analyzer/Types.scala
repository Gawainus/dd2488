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

  case object TLong extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TLong => true
      case _ => false
    }
    override def toString = "Long"
  }

  case object TIntArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TIntArray => true
      case _ => false
    }
    override def toString = "IntArray"
  }


  case object TDouble extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TDouble => true
      case _ => false
    }
    override def toString = "Double"
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

  // Todo: fix this
  case class TClass(classSymbol: ClassSymbol) extends Type {
    def getClassName = classSymbol.name

    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case tCls: TClass =>
        val targetName = tCls.getClassName
        if (getClassName.equals(targetName)) {
          true
        }
        else {
          var matched = false
          var parent= classSymbol.parent
          while (classSymbol.parent.isDefined && !matched) {
            if (parent.get.name.equals(targetName)) {
              matched = true
            }
            parent = classSymbol.parent
          }
          matched
        }

      case _ =>
        false
    }

    override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  val anyObject = TClass(ClassSymbol("Object"))
}

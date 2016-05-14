package slacc
package analyzer

import slacc.analyzer.Types._
import slacc.utils._

object Symbols {
  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol.")
    }

    private var _accessCount: Int = 0
    def getAccessCount: Int = _accessCount
    def increAccessCount: Unit = {
      _accessCount += 1
    }

  }

  class GlobalScope {
    var mainClass: ClassSymbol = _
    var classes = Map[String, ClassSymbol]()

    def lookupClass(k: String): Option[ClassSymbol] = {
      classes get k
    }
  } // end of class GlobalScope
  
  object GlobalScope {
    def apply(): GlobalScope = new GlobalScope()
  }

  sealed abstract class Symbol extends Positioned with Typed {
    val id: Int = ID.next
    val name: String
  } // end of class Symbol

  class ClassSymbol(val name: String) extends Symbol {

    // Todo: May need to define the apply method
    def apply(ClassSymbol: ClassSymbol) = ???

    var parent: Option[ClassSymbol] = None
    var members = Map[String, VariableSymbol]()
    var methods = Map[String, MethodSymbol]()

    def lookupMethod(k: String): Option[MethodSymbol] = {
      methods get k
    }
    def lookupVar(k: String): Option[VariableSymbol] = {
      members get k
    }
  } // end of class ClassSymbol

  object ClassSymbol {
    def apply(name: String): ClassSymbol = new ClassSymbol(name)
  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
    var params = Map[String, VariableSymbol]()
    var members = Map[String, VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None

    def lookupVar(k: String): Option[VariableSymbol] = {members get k}
  } // end of class MethodSymbol

  object MethodSymbol {
    def apply(name: String, classSymbol: ClassSymbol): MethodSymbol = new MethodSymbol(name, classSymbol)
  }

  class VariableSymbol(val name: String) extends Symbol
  object VariableSymbol {
    def apply(name: String): VariableSymbol = new VariableSymbol(name)
  }


  private object ID {
    private var c: Int = 0

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }
} // enod of object Symbols
